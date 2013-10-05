-- | The Commands module contains a representation of a single Hexwax expandIO-USB command as documented in its datasheet, which you can find here: <http://www.firmwarefactory.com/Docs/expandIO-USB%20HW148.pdf>

module Commands
       (
         HWCmd, HWMsg, CmdId, Payload,
         hwCmd,
         cSETSERIAL,
         cGETFWID,
         cINTERRUPT,
         cGETANALOG,
         cGETREG,
         cSETREG,
         cGETBIT,
         cSETBIT,
         cGETPORT,
         cSETPORT,
         cGETPORTBIT,
         cSETPORTBIT,
         cEXEI2C,
         cWAIT,
         cSCANMATRIX,
         cCAMULTIPLEX,
         cMPXDATA,
         cSTREAM,
         cCCMULTIPLEX,
         cEXESPI,
         cEXEUNIO,
         cERROR,
         cNOP
       )
       where


import Data.ByteString        as B hiding (putStrLn, find)
import Data.Word              (Word16, Word8)
import Data.List              (find)
import Prelude                as P
import Text.Printf            as T (printf)


-- | This holds a single Hexwax command as documented in the
-- datasheet.

type CmdId = Word8
type Payload = [Word8]


data HWCmd = HWCmd CmdId Payload


-- | This type is just a list of commands; used to build a single
-- pipelined request for better efficiency or for a specific reason
-- e.g. localised delay routines. The FS (full-speed) device can
-- accept up to 16 commands in a single request.

type HWMsg = [HWCmd]

cmdLenMax :: Int
cmdLenMax = 16


-- | The show implementation naively assumes that the payload length
-- is correct for the type of command. It attempts to also render a
-- meaningful display for the 'cGETFWID' command by intrepreting the
-- payload as indicated in the datasheet with respect to revision
-- number and device indentification.

instance Show HWCmd where
  show (HWCmd id bytes)
        | id == cGETFWID =
            printf "FirmwareId: %s rev %04d"
              (device $ bytes!!0) (revision bytes)
        |
          otherwise =
            printf "%s %s" (asCmdLabel id) (show bytes)
        where
          device 0x14 = "14K50"
          device 0x25 = "18F2455"
          device x    = "dev? " ++ (show x)
          revision b  = ((b!!1) * 8) + (b!!2)


hwCmd :: Word8 -> [Word8] -> HWCmd
hwCmd id bytes
  | P.length bytes > 3 = error "Too much data"
  | otherwise =
    case matchId id of
      Nothing    -> error $ "Unknown command id " ++ (show id)
      Just (v,_) -> HWCmd id $ [v] ++ bytes


asCmdId :: String -> Word8
asCmdId name = case matchName name of
  Nothing    -> error "Invalid command code"
  Just (v,_) -> v


asCmdLabel :: Word8 -> String
asCmdLabel cmd = case matchId cmd of
  Nothing -> "*???*"
  Just (_,label) -> label


matchName name = find (\(_,s) -> s == name) cmdSet
matchId   id   = find (\(c,_) -> c == id) cmdSet


-- | document these later, much later!
cSETSERIAL   = 0x93 ::Word8
cGETFWID     = 0x94 ::Word8
cINTERRUPT   = 0x95 ::Word8
cGETANALOG   = 0x96 ::Word8
cGETREG      = 0x98 ::Word8
cSETREG      = 0x99 ::Word8
cGETBIT      = 0x9A ::Word8
cSETBIT      = 0x9B ::Word8
cGETPORT     = 0x9C ::Word8
cSETPORT     = 0x9D ::Word8
cGETPORTBIT  = 0x9E ::Word8
cSETPORTBIT  = 0x9F ::Word8
cEXEI2C      = 0xA0 ::Word8
cWAIT        = 0xA9 ::Word8
cSCANMATRIX  = 0xAA ::Word8
cCAMULTIPLEX = 0xAB ::Word8
cMPXDATA     = 0xAC ::Word8
cSTREAM      = 0xAD ::Word8
cCCMULTIPLEX = 0xAE ::Word8
cEXESPI      = 0xAF ::Word8
cEXEUNIO     = 0xB0 ::Word8
cERROR       = 0xFF ::Word8
cNOP         = 0x00 ::Word8


cmdSet :: [(Word8, String)]
cmdSet =
  [(cSETSERIAL   , "SETSERIAL")
  ,(cGETFWID     , "GETFWID")
  ,(cINTERRUPT   , "INTERRUPT")
  ,(cGETANALOG   , "GETANALOG")
  ,(cGETREG      , "GETREG")
  ,(cSETREG      , "SETREG")
  ,(cGETBIT      , "GETBIT")
  ,(cSETBIT      , "SETBIT")
  ,(cGETPORT     , "GETPORT")
  ,(cSETPORT     , "SETPORT")
  ,(cGETPORTBIT  , "GETPORTBIT")
  ,(cSETPORTBIT  , "SETPORTBIT")
  ,(cEXEI2C      , "EXEI2C")
  ,(cWAIT        , "WAIT")
  ,(cSCANMATRIX  , "SCANMATRIX")
  ,(cCAMULTIPLEX , "CAMULTIPLEX")
  ,(cMPXDATA     , "MPXDATA")
  ,(cSTREAM      , "STREAM")
  ,(cCCMULTIPLEX , "CCMULTIPLEX")
  ,(cEXESPI      , "EXESPI")
  ,(cEXEUNIO     , "EXEUNIO")
  ,(cERROR       , "ERROR")
  ,(cNOP         , "NOP")
  ]


asPort :: Word8 -> String
asPort 0x01 = "PORTA"
asPort 0x02 = "PORTB"
asPort 0x03 = "PORTC"
asPort 0x04 = "PORTD"
asPort 0x05 = "PORTE"
asPort _    = "*ERROR*"



-- some sample command instances for console testing

fw1 = HWCmd cGETFWID [0x14, 0x00, 0x01]
fw2 = HWCmd cGETFWID [0x25, 0x00, 0x0a]


-- GETPORT PORTA=0x01 succ. byte2 = actual value on return

gp1 = HWCmd cGETPORT [0x01, 0x00, 0x55]
sp1 = HWCmd cSETPORT [0x01, 0x00, 0x55]
sp2 = HWCmd cSETPORT [0x02, 0x3e, 0x55]
