-- | The Hexwax module contains a simple interface to the ExpandIO-USB
-- range of devices. It absolves the caller of requiring any USB
-- knowledge and provides a quick and simple set of function calls
-- that allow the device to be embedded into a working project.
--
-- The main goal of this library is to allow working code to be
-- developed as quickly as possible to which end the only technical
-- knowledge required is to be able to plug the device into a USB
-- socket!

module Hexwax
       (
         -- * Attaching to a Hexwax device
         attach,
         attachTo,
         attachDummy,

         -- * Datasheet Command Set
         firmwareId,
         setPort,
         setPortBit,
         setRegister,
         getPort,
         getPortBit,
         getRegister,
         wait,

         -- * General USB enumeration
         -- ** Find handle by (product,vendor) id.
         handleFor,
         -- ** Fetch all devices with (product,vendor) id.
         handlesFor,
         -- * Utility Functions
         printUsbDeviceInfo,
         printUsbDeviceInfo2,
         printCmd,

         HWHandle, hwhDev, hwhMode, hwrBuf,
         Mode,

         -- * Register Constants
         ioPORTA,   ioPORTB,   ioPORTC,    ioPORTD,    ioPORTE,
         regPORTA,  regPORTB,  regPORTC,   regPORTD,   regPORTE,
         regTRISA,  regTRISB,  regTRISC,   regTRISD,   regTRISE,
         regCMCON,  regADCON1, regINTCON,  regINTCON2, regINTCON3,
	 regT0CON,  regT1CON,  regT2CON,   regT3CON,
         regCCPR1H, regCCPR1L, regCCPR2H,  regCCPR2L,  regPR2,
         regCCP1AS, regCCP1CON,regCCP1DEL, regCCP2CON,
         regLATA,   regLATB,   regLATC,    regLATD,    regLATE
       )
       where


import Control.Applicative
import Control.Exception      (Exception, throwIO)
import Control.Monad.MissingM as M
import Data.ByteString        as B hiding (putStrLn)
import Data.Vector            as V (Vector, filterM, find, null, toList, (!))
import Data.Word              (Word16, Word8)
import System.USB             as USB
import Text.Printf            as T (printf)
--
-- Hexwax command wrapper
--
import Commands               as C






-- | Read / write timeout. This ought to be more than enough time to
-- send a single 4-byte command using interrupt mode and get the
-- response.

rwTimeout :: Timeout
rwTimeout = 5000




-- | The spec says that the packet size should be padded out to 64
-- bytes, allowing up to 16 commands per packet. Each command sent
-- will have its response in the corresponding position in the
-- received packet.

pktLength :: Int
pktLength = 64




-- | This is the API specified length of a single command.

cmdLength :: Int
cmdLength = 4



-- | This is the padding data to make a command the required
-- size. Some commands are shorter than others so this makes padding
-- easy by using B.concat and taking the first 64 bytes from the
-- result.

cmdPad :: ByteString
cmdPad = B.replicate 63 0x00




-- | PVCode represents the product and vendor identifier codes as a
-- Word16 tuple.

type PVCode = (Word16, Word16)




-- | The default product and vendor id for an off-the-shelf chip.

defaultHexwaxId :: PVCode
defaultHexwaxId = (0x0B40, 0x0132)




-- | StdMode implies normal quiet operation and DebugMode means output lots of
-- noisy information to stdout.

data Mode = StdMode | DebugMode deriving Show




-- | HWHandle is the way in which application code communicates with
-- the Hexwax interface. It contains the operating 'Mode' and other
-- information required by the library.

data HWHandle = HWHandle
                { hwhDev :: Device
                , hwhMode :: Mode
                }
              deriving (Show)




-- | This is the Hexwax response packet to the calling application
-- code. If the command failed we hand back the status code from the
-- USB layer, if it worked we hand back a 4-byte command response.

data HWResponse = HWFailed Status
                | HWFirmwareId { fwDevice :: Int, fwVersion :: Int }
                | HWOK { hwrBuf :: ByteString }
                deriving (Show)




-- | Shortcut for the response from a command instruction.

type CmdResponse = Either (Size, Status) (ByteString, Status)




-- | The normal scenario is going to be: lone-hacker, one-device, just
-- get me started fast. This function then is all you should need in
-- that situation assuming you have not changed the vendor and product
-- codes in the device and the device is properly powered etc.

attach :: IO (Maybe HWHandle)
attach = attachTo defaultHexwaxId StdMode


-- | Attach in debug mode. Lots of tracking output to stdout.
attachDebug :: IO (Maybe HWHandle)
attachDebug = attachTo defaultHexwaxId DebugMode


-- | As debug mode but no real hardware has to be plugged in. This is
-- intended for debugging the application code in the absence of the
-- actual device.

attachDummy :: IO (Maybe HWHandle)
attachDummy = attachTo (0,0) DebugMode


-- | Given the vendor id and product id this will attempt to locate
-- the device and return a handle that can be used to send commands to
-- it. This handle does not need management by the caller i.e. no
-- release calls etc. are required.

attachTo :: PVCode -> Mode -> IO (Maybe HWHandle)
attachTo (0,0) mode = return $ Just (HWHandle undefined mode)
attachTo (idVendor, idProduct) mode = do
  ctx <- USB.newCtx
  device <- handleFor ctx (idVendor, idProduct)
  case device of
    Nothing -> return Nothing
    Just device -> return $ Just (HWHandle device mode)




-- | This is the core of all of the commands: it executes the
-- send-receive packet exchange and translates the USB response into
-- either 'HWFailed' or 'HWOK'

cmdWorker :: Device -> ByteString -> IO HWResponse
cmdWorker device cmd = do
  result <- hexwaxCmd device cmd
  case result of
    Left (size, status)  -> return $ HWFailed status
    Right (buffer, size) -> return $ HWOK $ B.take 4 buffer




hexwaxCmd :: Device -> ByteString -> IO CmdResponse
hexwaxCmd device cmdBuf = do
  withDeviceHandle device (attach 0)
  where attach :: InterfaceNumber -> DeviceHandle -> IO CmdResponse
        attach ifNum handle = do
          -- OSX: Will require a codeless-kext to achieve this as
          -- libusb has a stub for "detach_kernel_driver()".
          withDetachedKernelDriver handle ifNum claim
          where claim = do
                  withClaimedInterface handle ifNum execWrite
                  where
                    execWrite = do
                      writeCmd cmdBuf ifNum handle




-- | Write a four-byte command string to the Hexwax device. A five
-- second timeout is applied and the endpoint is fixed to "1" as
-- indicated in the Hexwax documentation. If the write is good (4
-- bytes are sent) then the response will be "Right (ByteString,
-- Status)" reflecting on what was read back as the response. If the
-- write fails then we can expect "Left (Size, Status)" reflecting the
-- result of the initial write attempt.

writeCmd :: ByteString -> InterfaceNumber -> DeviceHandle -> IO CmdResponse
writeCmd cmdBuf ifNum handle
  | B.length cmdBuf >= 1 = do
    --printCmd "TXD:" cmdBuf
    response <- intWriter padCmd rwTimeout
    case response of
      (pktLength, Completed) -> do
        readBack@(buffer, status) <- intReader pktLength rwTimeout
        --putStrLn $ (show status) ++ " => " ++ (show buffer) ++ "\n"
        --printCmd "RXD:" buffer
        return $ Right (buffer, status)
      (sz, st) -> return $ Left (sz, st)

  | otherwise =
      return $ Left (-1, TimedOut) -- need my own exceptions at this point
  where
    intReader = readInterrupt handle $ EndpointAddress 1 In
    intWriter = writeInterrupt handle $ EndpointAddress 1 Out
    padCmd    = B.take pktLength (B.concat [cmdBuf, cmdPad])




-- | Wait a while. The identifier WAIT (0xA9) waits until a register
-- bit changes or a timeout occurs. Byte 1 indicates the register and
-- byte 2 bits 0-2 indicates the bit (0-7). Byte 2 bit 7 is 0 for wait
-- until clear or 1 for wait until set. Byte 3 is the timeout in
-- milliseconds. Actual timeout might take slightly more time than
-- specified, but never less.
--
-- In the response, byte 3 will be the number of milliseconds
-- remaining before the timeout would have occurred, or 0 if it did
-- occur. The timeout is required, as USB commands are not processed
-- during wait periods.

wait :: HWHandle -> Word8 -> Word8 -> Word8 -> IO HWResponse
wait h regId bitNum timeout =
  cmdWorker (hwhDev h) (B.pack [iWAIT, regId, bitNum, timeout])




-- | Set one of the PORTA/B/C ports. The identifier SETPORT (0x9D)
-- sets the value of a port output latch. Byte 1 indicates the port
-- (A=1, B=2...). Byte 2 specifies the new value. Byte 3 specifies
-- which bits should be set as outputs ('0') and which bits should be
-- set as inputs ('1').

setPort :: HWHandle -> Word8 -> Word8 -> Word8 -> IO HWResponse
setPort h port value dirFlags =
  cmdWorker (hwhDev h) (B.pack [iSETPORT, port, value, dirFlags])




-- | Get a value from one of the PORTA/B/C registers. The identifier
-- GETPORT (0x9C) retrieves value of a port. Byte 1 indicates the port
-- (A=1, B=2...). Byte 3 specifies which bits should be set as outputs
-- (‘0’) and which bits should be set as inputs (‘1’). The response is
-- the same as the command, except the port value is given in byte 2.
-- (Note: Byte2 is not used in the outbound command, fixed to 0x00)

getPort :: HWHandle -> Word8 -> Word8 -> IO HWResponse
getPort h regId dir = cmdWorker (hwhDev h) (B.pack [iGETPORT, regId, 0x00, dir])




-- | Set a single bit on a register. The identifier SETPORTBIT (0x9F) sets
-- or clears a single bit of a port output latch. Byte 1 indicates the
-- port (A=1, B=2...) and byte 2 indicates the bit (0-7). Byte 3 bit 0
-- is 0 for clear and 1 for set. Byte 3 bit 1 is 0 if the bit should
-- be configured as an output and 1 if it should be configured as an
-- input. The response is the same as the command.

setPortBit :: HWHandle -> Word8 -> Word8 -> Word8 -> IO HWResponse
setPortBit h port bitNum onOrOff =
  cmdWorker (hwhDev h) (B.pack [iSETPORTBIT, port, bitNum, onOrOff])




-- | Get a single bit from a register. The identifier GETBIT (0x9A)
-- retrieves a single bit from a micro-controller register. Byte 1
-- indicates the register as detailed in appendix I and byte 2
-- indicates the bit (0-7). In the response, byte 3 is 0 for clear and
-- 1 for set.

getPortBit :: HWHandle -> Word8 -> Word8 -> IO HWResponse
getPortBit h regId bitNum =
  cmdWorker (hwhDev h) (B.pack [iGETPORTBIT, regId, bitNum])




-- | The identifier SETREG (0x99) sets the value of a microcontroller
-- register. Byte 1 specifies the register as detailed in appendix
-- I. Byte 2 specifies the new value. The response is the same as the
-- command. Directly accessing registers requires in-depth knowledge
-- of the base microcontroller, but it provides greater flexibility
-- than the other commands. Refer to the base microcontroller data
-- sheet for details.

setRegister:: HWHandle -> Word8 -> Word8 -> IO HWResponse
setRegister h regId value = cmdWorker (hwhDev h) (B.pack [iSETREG, regId, value])




-- | The identifier GETREG (0x98) retrieves value of a microcontroller
-- register. Byte 1 specifies the register as detailed in appendix
-- I. The response is the same as the command, except the register
-- value is given in byte 2. Directly accessing registers requires
-- in-depth knowledge of the base microcontroller, but it provides
-- greater flexibility than the other commands. Refer to the base
-- microcontroller data sheet for details.

getRegister:: HWHandle -> Word8 -> IO HWResponse
getRegister h regId = cmdWorker (hwhDev h) (B.pack [iGETREG, regId])



-- | Get the firmware identification number. The identifier GETFWID
-- (0x94) retrieves the firmware version number. In the response the
-- device is in byte 1 (0x14 for 14K50, 0x25 for 2455, etc) and a
-- version number is in bytes 2 (MSB) and 3 (LSB).

firmwareId :: HWHandle -> IO HWResponse
firmwareId h = cmdWorker (hwhDev h) (B.pack [iGETFWID])




-- | Given the product and vendor identification numbers this will
-- scan all of the available USB devices exposed by the given USB
-- context and then return either the matching Device handle or
-- Nothing. If there are multiple devices plugged in then it will
-- return the first one that matches.

handleFor :: Ctx -> (Word16, Word16) -> IO (Maybe Device)
handleFor ctx dev@(cVendor, cProd) =
  getDevices ctx >>= \devs -> M.findM (isTarget dev) $ V.toList devs




-- | Returns a list of all of the matching USB devices even if that
-- list is empty. The caller is expected to have initialised a USB
-- session prior to calling.

handlesFor :: Ctx -> (Word16, Word16) -> IO (Vector Device)
handlesFor ctx dev@(vendorId, productId) =
  getDevices ctx >>= \devs -> V.filterM (isTarget dev) devs




-- | Internal helper to simply check that the product and vendor
-- values in the current Device handle match those in (vid, pid).

isTarget :: (Word16, Word16) -> Device -> IO Bool
isTarget comp@(vid, pid) dev = do
  getDeviceDesc dev >>= \info ->
    return $ (deviceVendorId info, deviceProductId info) == comp




-- | Given a Device instance this will produce a readable text output
-- to stdout showing the values of the various fields. This can be
-- used with any USB device handle not just one associated with a
-- Hexwax device.

printUsbDeviceInfo :: Device -> IO ()
printUsbDeviceInfo dev = do
  desc <- getDeviceDesc dev
  putStrLn $ "DEVICE: " ++ (show dev)
  putStrLn $ "usb-release-number:  " ++ (show $ deviceUSBSpecReleaseNumber desc)
  putStrLn $ "class:               " ++ (show $ deviceClass desc)
  putStrLn $ "subclass:            " ++ (show $ deviceSubClass desc)
  putStrLn $ "protocol:            " ++ (show $ deviceProtocol desc)
  putStrLn $ "max-packet-size-ep0: " ++ (show $ deviceMaxPacketSize0 desc)
  putStrLn $ "vendor-id:           " ++ (show $ deviceVendorId desc)
  putStrLn $ "product-id:          " ++ (show $ deviceProductId desc)
  putStrLn $ "release-number:      " ++ (show $ deviceReleaseNumber desc)
  putStrLn $ "manufacturer-strIx:  " ++ (show $ deviceManufacturerStrIx desc)
  putStrLn $ "product-strIx:       " ++ (show $ deviceProductStrIx desc)
  putStrLn $ "serial-number-strIx: " ++ (show $ deviceSerialNumberStrIx desc)
  putStrLn $ "no.-configurations:  " ++ (show $ deviceNumConfigs desc)

printUsbDeviceInfo2 :: Device -> IO ()
printUsbDeviceInfo2 dev = do
  desc <- getDeviceDesc dev
  putStrLn $ "DEVICE: " ++ (show dev)
  putStrLn $ "usb-release-number:  " ++ (show $ deviceUSBSpecReleaseNumber desc)
  putStrLn $ "class:               " ++ (show $ deviceClass desc)
  putStrLn $ "subclass:            " ++ (show $ deviceSubClass desc)
  putStrLn $ "protocol:            " ++ (show $ deviceProtocol desc)
  putStrLn $ "max-packet-size-ep0: " ++ (show $ deviceMaxPacketSize0 desc)
  putStrLn $ "vendor-id:           " ++ (show $ deviceVendorId desc)
  putStrLn $ "product-id:          " ++ (show $ deviceProductId desc)
  putStrLn $ "release-number:      " ++ (show $ deviceReleaseNumber desc)
  putStrLn $ "manufacturer-strIx:  " ++ (show $ deviceManufacturerStrIx desc)
  putStrLn $ "product-strIx:       " ++ (show $ deviceProductStrIx desc)
  putStrLn $ "serial-number-strIx: " ++ (show $ deviceSerialNumberStrIx desc)
  putStrLn $ "no.-configurations:  " ++ (show $ deviceNumConfigs desc)

printCmd :: String -> ByteString -> IO ()
printCmd sol buf =
  T.printf "%s %s / %02X " sol (cmdLabel op) op >> printBuf (B.tail buf)
  where op = (B.index buf 0)


printBuf :: ByteString -> IO ()
printBuf buf = do
  case B.null buf of
    True  -> T.printf "\n"
    False -> T.printf "%02X " (B.index buf 0) >> printBuf (B.tail buf)




-- | Instruction codes used to send USB commands to the device.

iNOP         = 0x00
iSETSERIAL   = 0x93
iGETFWID     = 0x94
iINTERRUPT   = 0x95
iGETANALOG   = 0x96
iGETREG      = 0x98
iSETREG      = 0x99
iGETBIT      = 0x9A
iSETBIT      = 0x9B
iGETPORT     = 0x9C
iSETPORT     = 0x9D
iGETPORTBIT  = 0x9E
iSETPORTBIT  = 0x9F
iEXEI2C      = 0xA0
iWAIT        = 0xA9
iSCANMATRIX  = 0xAA
iCAMULTIPLEX = 0xAB
iMPXDATA     = 0xAC
iSTREAM      = 0xAD
iCCMULTIPLEX = 0xAE
iEXESPI      = 0xAF
iEXEUNIO     = 0xB0

-- | Map a command code to a string. used for display purposes.

cmdLabel :: Word8 -> String
cmdLabel 0x93 = "SETSERIAL"
cmdLabel 0x94 = "GETFWID"
cmdLabel 0x95 = "INTERRUPT"
cmdLabel 0x96 = "GETANALOG"
cmdLabel 0x98 = "GETREG"
cmdLabel 0x99 = "SETREG"
cmdLabel 0x9A = "GETBIT"
cmdLabel 0x9B = "SETBIT"
cmdLabel 0x9C = "GETPORT"
cmdLabel 0x9D = "SETPORT"
cmdLabel 0x9E = "GETPORTBIT"
cmdLabel 0x9F = "SETPORTBIT"
cmdLabel 0xA0 = "EXEI2C"
cmdLabel 0xA9 = "WAIT"
cmdLabel 0xAA = "SCANMATRIX"
cmdLabel 0xAB = "CAMULTIPLEX"
cmdLabel 0xAC = "MPXDATA"
cmdLabel 0xAD = "STREAM"
cmdLabel 0xAE = "CCMULTIPLEX"
cmdLabel 0xAF = "EXESPI"
cmdLabel 0xB0 = "EXEUNIO"
cmdLabel 0xFF = "ERROR"
cmdLabel 0x00 = "NOP"
cmdLabel aVal = "HUH? " ++ (show aVal)




-- | These are for when a PORT is required, mainly GETPORT, SETPORT,
-- GETPORTBIT and SETPORTBIT.

ioPORTA = 1 :: Word8
ioPORTB = 2 :: Word8
ioPORTC = 3 :: Word8
ioPORTD = 4 :: Word8
ioPORTE = 5 :: Word8




-- | These are the more generic register identification codes used for
-- most of the other instructions in the set.

regPORTA   = 0x80 :: Word8
regPORTB   = 0x81 :: Word8
regPORTC   = 0x82 :: Word8
regPORTD   = 0x83 :: Word8
regPORTE   = 0x84 :: Word8

regLATA    = 0x89 :: Word8
regLATB    = 0x8A :: Word8
regLATC    = 0x8B :: Word8
regLATD    = 0x8C :: Word8
regLATE    = 0x8D :: Word8

regCCP1AS  = 0xB7 :: Word8
regCCP1CON = 0xBD :: Word8
regCCP1DEL = 0xB6 :: Word8
regCCP2CON = 0xBA :: Word8
regCCPR1H  = 0xBF :: Word8
regCCPR1L  = 0xBE :: Word8
regCCPR2H  = 0xBC :: Word8
regCCPR2L  = 0xBB :: Word8

regADCON1  = 0xC1 :: Word8
regPR2     = 0xCB :: Word8

regT0CON   = 0xD5 :: Word8
regT1CON   = 0xCD :: Word8
regT2CON   = 0xCA :: Word8
regT3CON   = 0xB1 :: Word8

regINTCON  = 0xF2 :: Word8
regINTCON2 = 0xF1 :: Word8
regINTCON3 = 0xF0 :: Word8

regCMCON   = 0xB4 :: Word8



-- | Tri-state control registers. These control which corresponding
-- PORTx pins are inputs and output.

regTRISA = 0x92 :: Word8
regTRISB = 0x93 :: Word8
regTRISC = 0x94 :: Word8
regTRISD = 0x95 :: Word8
regTRISE = 0x96 :: Word8
