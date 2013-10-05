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


-- | This will become another command line option at some point.  It
-- defines the mark-space ratio of the stepper clock pulse.
stepDelay = threadDelay 1

stepperPort  = ioPORTB     -- output port to controller
bitDir       = 6 :: Word8  -- direction control bit
bitClk       = 7 :: Word8  -- stepper clock bit
dirForwards  = 0 :: Word8  -- fwd / cw
dirBackwards = 1 :: Word8  -- bwd / ccw


--stepperPort  = ioPORTA     -- output port to controller
--bitDir       = 1 :: Word8  -- direction control bit
--bitClk       = 0 :: Word8  -- stepper clock bit

-- | Header used for the help / info output
helpHeader = "Usage is: smc [OPTION...]"
helpText   =  usageInfo helpHeader options

type Frequency = Int
type DutyCycle = Int

--data HWBoard = DummyBoard | RealBoard dev

data Flag = Forward Int
          | Backward Int
          | Wait Int
          | Sleep Int
          | Pwm Frequency
          | LedMode
          | Script
--        | HWBoard
          | Help
          | Info
          deriving (Show)


isEnv Help    = True
isEnv Info    = True
isEnv LedMode = True
--isEnv (RealBoard _) = True
isEnv _       = False


-- | This list contains all of the command line options that the
-- program understands for controlling the stepper motor.

options :: [OptDescr Flag]
options =
  [ Option ['h'] ["help"]    (NoArg Help)              "Show help information"
  , Option ['i'] ["info"]    (NoArg Info)              "Show big help text"
  , Option ['s'] ["script"]  (NoArg Script)            "Execute smc script file"
  , Option ['f'] ["fwd"]     (OptArg asFwd   "?fwd")   "Step forward"
  , Option ['b'] ["bwd"]     (OptArg asBwd   "?bwd")   "Step backward"
  , Option ['w'] ["wait"]    (OptArg asWait  "100")    "Wait N mS"
  , Option ['p'] ["pwm"]     (OptArg asPwm   "?pwm")   "Set PWM frequency with 50% duty cycle"
--  , Option ['n'] ["noboard"] (OptArg asBoard "false")  "use dummy hardware mode, no board required"
  ]


asFwd :: Maybe String -> Flag
asFwd Nothing       = Forward 1
asFwd (Just nSteps) = Forward (read nSteps :: Int)

asBwd :: Maybe String -> Flag
asBwd Nothing       = Backward 1
asBwd (Just nSteps) = Backward (read nSteps :: Int)

--asDir :: Flag -> (Maybe String -> Flag)

asWait :: Maybe String -> Flag
asWait Nothing = Wait 100
asWait (Just n) = Wait (read n :: Int)


asPwm :: Maybe String -> Flag
asPwm Nothing = Pwm 0
asPwm (Just n) = Pwm (read n :: Int)

{-
asBoard :: Maybe String -> Flag
asBoard Nothing  = HWBoard False
asBoard (Just n) = do
  case (read n :: Int) of
    1 -> return (HWDummy)
    _ -> return ()
-}
      



-- | Stepper Motor Controller. This program is a small utility that is
-- designed to work with our prototype board. It understands a couple
-- of command line options and will help us get the control circuitry
-- debugged and up-and-running.

main :: IO ()
main = do
  --
  -- Print the sign-on banner and then decode the command line options
  -- that have been supplied.
  --
  putStrLn "Stepper Motor Controller v 1.0 (C) 2013 Objitsu / Sean James Charles"
  putStrLn "Hint: Use -i, -h, --info or --help for more information\n"
  args <- getArgs
  (opts, _) <- compilerOpts args
  print opts
  --
  -- Split options into "pre-execution settings" and "code". The last
  -- thing performed is the establishing of a USB environment and the
  -- final value from this is the Hexwax device, real or not-there...
  --
  let (env,script) = partition isEnv opts
  print env
  print script
  -- fold' => end up with device "dev"

  mapM_ (\opt -> doOption undefined opt) env
  
  --
  -- Initiate a connection to the expandIO-USB now and then execute
  -- the sequence of smc instructions.
  --
  hexwax <- HW.attach

  case hexwax of
    Nothing -> do
      putStrLn "Couldn't locate the expandIO-USB device, is it plugged in?"
      exitWith $ ExitFailure 1

    Just hwHandle -> do
      putStrLn "Located the expandIO-USB, executing instructions.\n"
      -- filter options into those that affect the environment and
      -- those that constitute the actual programming script. If any
      -- of the env commands cause a system exit then so be it!
      mapM_ (\opt -> doOption hwHandle opt) env
      putStrLn "> PROGRAM START"
      mapM_ (\opt -> doOption hwHandle opt) script
      putStrLn "> PROGRAM END"


-- | Using the getOpt library this function will return a tuple
-- consisting of the known arguments that were given and the
-- remainder. If nothing at all is passed on the comman dline we
-- simulate "-h" for assistance.

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv =
  case getOpt Permute options argv of
    ([], [], []) -> return ([Help], [])
    (o, n, [])   -> return (o, n)
    (_, _, errs) -> ioError (userError (concat errs ++ helpText))



-- | This will display a large and informative block of text by
-- reading and displaying the contents of the file "smc.help" which is
-- expected to be in the same folder as the executable until further
-- notice. Once that's done it terminates.

doOption :: HWHandle -> Flag  -> IO ()
doOption _ Info = do
  helpText <- readFile "smc.help"
  putStrLn helpText
  exitWith ExitSuccess

doOption _ Help = putStrLn $ usageInfo helpHeader options



-- | Wait pauses this application for specified number of
-- milliseconds. No other instructions are executed whilst the delay
-- is in progress so change-of-states etc. might have been missed.

doOption dev (Wait n) = do
  putStrLn $ "> WAIT " ++ (show n) ++ " mS"
  threadDelay (n*1000)



-- | Execute an smc script from the given file. This option gives rise
-- to a much more powerful and flexible way of programming stepper
-- motors.

doOption _ Script = do
  putStrLn "Script execution"



-- | Step the motor forwards / clockwise assuming that it is wired
-- that way, This may in fact go the opposite way depending upon how
-- you have constructed your hardware.

doOption dev (Forward n)  = do
  putStrLn $ "> STEP FORWARD " ++ (show n)
  stepIt dev n dirForwards



-- doOption _ (RealBoard False) = putStrLn "Dummy hardware mode assumed"
-- doOption _ (RealBoard True)  = return ()


-- | Step the motor backwards / counter-clockwise assuming that it is
-- wired that way, This may in fact go the opposite way depending upon
-- how you have constructed your hardware.

doOption dev (Backward n) = do
  putStrLn $ "> STEP BACKWARD " ++ (show n)
  stepIt dev n dirBackwards


--doOption _ LedMode = putStrLn "LED mode activated on PORTA"

doOption dev (Pwm freq) = do
      let setReg = HW.setRegister dev -- presto! My own mini ASM language!
      setReg regTRISC   0x00 -- port C all output
      setReg regPORTC   0xFF -- clear port c

      case freq of
	0 ->
          setReg regCCP1CON 0x00 -- disable PWM mode, leds off

        _ -> do
	  setReg regPR2     0xBB
          setReg regT2CON   0x07
          setReg regCCPR1L  0x5D  -- 50% 0101 1101 11
          setReg regCCP1CON 0x3C  -- 0011 1100  => DC1,0 = 1,1 PWM enabled

      return ()


-- | Pulse the required number of steps in the selected direction.

stepIt :: HWHandle -> Int -> Word8 -> IO ()
stepIt dev n dir = do
  HW.setPortBit dev stepperPort bitDir dir
  return ()
  