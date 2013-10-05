module Main (main) where

import Control.Concurrent     (threadDelay)
import Control.Monad          (forM_)
import Data.List              (partition)
import Data.Maybe             (fromMaybe)
import Data.Word              (Word16, Word8)
import Hexwax                 as HW
import System.Console.GetOpt
import System.Environment     (getArgs)
import System.Exit

-- | The clock frequency of our board.
fCLK :: Int
fCLK = 12000000

-- | The duty cycle we want for the stepper output
dutyCycle :: Int
dutyCycle = 50 -- per cent

preScale :: Int
preScale = 16

main :: IO ()
main = do
  hexwax <- HW.attach

  case hexwax of
    Nothing -> do
      putStrLn "Couldn't locate the expandIO-USB device, is it plugged in?"
      exitWith $ ExitFailure 1

    Just dev -> do
      let setReg = HW.setRegister dev -- presto! My own mini ASM language!
      --
      -- ASM begin
      --
      setReg regTRISC   0x00 -- port C all output
      setReg regPORTC   0xFF -- clear port c

      setReg regCCP1CON 0x00 -- disable PWM mode, leds off

      -- 
      --setReg regPR2     0xBB

      -- TIMER2 CONTROL
      --setReg regT2CON   0x07

      -- DUTY CYCLE: The eight MOST_SIGNIFICANT bits of the duty cycle are
      -- set here, ten bits of resolution available.
      -- 10 bits = 0 to 1023, 50%
      --
      --setReg regCCPR1L  0x5D  -- 50% 0101 1101 11
      --setReg regCCPR1L  0x2D  -- 25% 0010 1101 11

      -- Final setup here is to configure the bottom two bits of the
      -- PWM DUTY CYCLE value and START PWN mode...
      --
      -- CCP1CON: 0 0 DC1 DC0 | 3 -0
      --                        0000 = CCP/PWN disabled (resets CCP)
      --                        11xx = PWM mode enabled
      --
      --setReg regCCP1CON 0x3C  -- 0011 1100  => DC1,0 = 1,1 PWM enabled

      --
      -- ASM end
      --
      putStrLn "Something should be happening..."


{-
/* http://www.micro-examples.com/public/microex-navig/doc/097-pwm-calculator.html
 * PWM registers configuration
 * Fosc = 12000000 Hz
 * Fpwm = 997.34 Hz (Requested : 1000 Hz)
 * Duty Cycle = 50 %
 * Resolution is 10 bits
 * Prescaler is 16
 * Ensure that your PWM pin is configured as digital output
 * see more details on http://www.micro-examples.com/
 * this source code is provided 'as is',
 * use it at your own risks
 */
PR2     = 0b 1011 1011 ;  0xBB
T2CON   = 0b 0000 0111 ;  0x07
CCPR1L  = 0b 0101 1101 ;  0x5D
CCP1CON = 0b 0011 1100 ;  0x3C

   25% duty-cycle

PR2     = 0b 1011 1011 ;
T2CON   = 0b 0000 0111 ;
CCPR1L  = 0b 0010 1110 ;
CCP1CON = 0b 0011 1100 ;
-}
