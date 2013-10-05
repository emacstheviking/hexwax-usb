module Main where
-- module System.USB.Hexwax
--import Data.Time.Clock

import Control.Concurrent     (threadDelay)
import Data.ByteString        as B hiding (putStrLn, getLine)
import Data.Char              (intToDigit)
import Data.Word              (Word16)
import Numeric                (showIntAtBase)
import System.Environment
import System.IO
import Hexwax                 as HW
import Text.Printf

main :: IO ()
main = do
  hw <- HW.attach
  case hw of
    Just dev -> do
      -- Print the FIRMWAREID response
      response <- HW.firmwareId dev
      -- Get the USB information printed out
      HW.printUsbDeviceInfo2 $ hwhDev dev
      -- set output unbuffered we we are using ANSI [-sequence
      hSetBuffering stdout NoBuffering
      --
      -- read all bits from PORTB, from datasheet: 9C 02 00 FF
      --
      -- 18Fx455 datasheetpage 116 EXAMPLE 10-2: INITIALIZING PORTB
      HW.setRegister dev regPORTB  0x00   -- clear output data latches
      HW.setRegister dev regADCON1 0x0E -- set RB<4:0> as digital I/I pins
      HW.setRegister dev regTRISB  0xFF -- set RB<7:0> as inputs
      --
      -- INTCON2<7? clear to ENABLE PORTB internal pull-ups via LATB
      -- DEFAULT STATE: () 1111 -1-1   1111 0101 => F5
      -- DESIRED STATE:    0111 -1-1   0111 0101 => E5
      --HW.setRegister dev regINTCON2 0xE5
      --HW.setRegister dev regLATB 0x00 -- LATB written as 0 ??!?!!?
      --HW.getRegister dev regTRISB >>= \x -> print x
      -- CMCON to 0x07 (Default value anyway!)
      --HW.setRegister dev 0xB4 0x07
      printOutput
      return ()
      where printOutput = do
              --HW.wait dev regPORTC 0x00 0xFF
              buf1 <- HW.getRegister dev regPORTB
              let val1 = (B.index (hwrBuf buf1) 2)
              --putStrLn $ "MSB " ++ (showIntAtBase 2 intToDigit val1 " LSB --PORTB")
              buf2 <- HW.getPort dev ioPORTB 0xFF
              printf "\ESC[sPORTB: %02X   %02X\ESC[u"
              --printf "PORTB: GETREG: %02X\n" --   GETPORT: %02X\n"
                (B.index (hwrBuf buf1) 2)
                (B.index (hwrBuf buf2) 2)
              --threadDelay 1000000
              printOutput

    Nothing ->
      putStrLn "HexWax device not found"


{-
      -- simple LED test
      response <- HW.setPort dev 0x01 0xFE 0x00
      putStrLn "Press a key" >> getLine
      response <- HW.setPort dev 0x01 0xFD 0x00
      putStrLn "Press a key" >> getLine
      response <- HW.setPort dev 0x01 0xFB 0x00
      putStrLn "Press a key" >> getLine
      response <- HW.setPort dev 0x01 0xF7 0x00
      putStrLn "Press a key" >> getLine
      response <- HW.setPort dev 0x01 0xFF 0x00 -- all bits off again
      putStrLn "*done*"
-}
{-
      -- set output unbuffered we we are using ANSI [-sequence
      hSetBuffering stdout NoBuffering
      -- read all bits from PORTB, from datasheet: 9C 02 00 FF
      -- Set portB as all inputs
      HW.setRegister dev regTRISB 0xFF
      HW.getRegister dev regTRISB >>= \x -> print x
      printOutput
      return ()
      where printOutput = do
              HW.wait dev regPORTC 0x00 0xFF
              buf1 <- HW.getRegister dev regPORTB
              buf2 <- HW.getPort dev ioPORTB 0xFF
              printf "\ESC[sPORTB: %02X   %02X\ESC[u"
                (B.index (hwrBuf buf1) 2)
                (B.index (hwrBuf buf2) 2)
              printOutput
-}

{-
      -- WAIT test... 4 * 250 is one second so we should be able to blink
      HW.setPortBit dev ioPORTA  0x00 0x00
      HW.wait       dev regPORTB 0x00 0xFA
      HW.wait       dev regPORTB 0x00 0xFA
      HW.wait       dev regPORTB 0x00 0xFA
      HW.wait       dev regPORTB 0x00 0xFA
      HW.wait       dev regPORTB 0x00 0xFA
      HW.wait       dev regPORTB 0x00 0xFA
      HW.wait       dev regPORTB 0x00 0xFA
      HW.wait       dev regPORTB 0x00 0xFA
      HW.wait       dev regPORTB 0x00 0xFA
      HW.wait       dev regPORTB 0x00 0xFA
      HW.wait       dev regPORTB 0x00 0xFA
      HW.wait       dev regPORTB 0x00 0xFA
      HW.setPortBit dev ioPORTA  0x00 0x01
-}
{-
      response <- HW.setPort dev 0x01 0xFE 0x00
      putStrLn "Press a key" >> getLine
      response <- HW.setPort dev 0x01 0xFD 0x00
      putStrLn "Press a key" >> getLine
      response <- HW.setPort dev 0x01 0xFB 0x00
      putStrLn "Press a key" >> getLine
      response <- HW.setPort dev 0x01 0xF7 0x00
      putStrLn "Press a key" >> getLine
      response <- HW.setPort dev 0x01 0xFF 0x00 -- all bits off again

      response <- HW.setPortBit dev 0x01 0x00 0x00
      putStrLn "Press a key" >> getLine
      response <- HW.setPortBit dev 0x01 0x00 0x01
      putStrLn "Press a key" >> getLine
      response <- HW.setPortBit dev 0x01 0x01 0x00
      putStrLn "Press a key" >> getLine
      response <- HW.setPortBit dev 0x01 0x01 0x01
      putStrLn "Press a key" >> getLine
      response <- HW.setPortBit dev 0x01 0x02 0x00
      putStrLn "Press a key" >> getLine
      response <- HW.setPortBit dev 0x01 0x02 0x01
      putStrLn "Press a key" >> getLine
      response <- HW.setPortBit dev 0x01 0x03 0x00
      putStrLn "Press a key" >> getLine
      response <- HW.setPortBit dev 0x01 0x03 0x01

      -- stepper motor pattern, one direction
      putStrLn "Press key, stepper motor pattern..." >> getLine
-}


{-
hexwaxGo2 :: Device -> IO ()
hexwaxGo2 device =  withDeviceHandle device attach
  where attach handle = withDetachedKernelDriver handle 0 claim
        claim = do
          let cmd = BS.replicate 4 '\0'
          withClaimedInterface handle ifNum
          (HW.writeCmd cmd ifNum handle)
-}

-- withDeviceHandle => withDetachedKernelDriver => withClaimedInterface => HW.writeCmd

{-
hexwaxGo :: Device -> IO ()
hexwaxGo device = do
  withDeviceHandle device attach
  where attach handle = do
          withDetachedKernelDriver handle 0 claim
          where claim = do
                  let cmd = BS.replicate 4 '\0'
                  withClaimedInterface handle ifNum
                    (HW.writeCmd cmd ifNum handle)
-}

{-
     System.USB.Hexwax
                  .open -- use default from datasheet
                  .openAt productId vendorId
                  .close
                  .API. as HWA
                      .setBit .getBit .setPort .getPort etc.
                  .Util as HWU
                      --
                      -- Bipolar stepper-motor driving
                      --
                      .bpStepFwd
                      .bpStepFwd N
                      .bpStepRev
                      .bpStepRev N
                      --
                      -- Unipolar stepper-motor driving
                      --
                      .upStepFwd
                      .ubpStepFwd N
                      .upStepRev
                      .upStepRev N


    clever extensions:

        the ability to name an arbitrary group of outputs as a "pseudo-register" and then
        send a value to it.
-}
