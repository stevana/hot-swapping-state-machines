module LibMain where

import System.IO
import System.FilePath
import System.Posix.Files
import Control.Exception
import Text.Read
import System.IO.Error

import StateMachine
import Code
import AbstractMachine

------------------------------------------------------------------------

data Command = Do Value | Load Code
  deriving (Show, Read)

pipePath :: FilePath
pipePath = "/tmp/freefunc"

libMain :: Code -> IO ()
libMain code0 = do
  safeCreateNamedPipe (pipePath <.> "command")
  safeCreateNamedPipe (pipePath <.> "response")
  withFile (pipePath <.> "command") ReadWriteMode $ \h -> do
    hSetBuffering h LineBuffering
    putStrLn "Waiting for commands..."
    go h code0 (Int 0)
  where
    go :: Handle -> Code -> Value -> IO ()
    go h code state = do
      s <- hGetLine h
      let mCmd = readMaybe s
      case mCmd of
        Nothing -> do
          putStrLn ("Invalid command: " ++ s)
          go h code state
        Just (Load code') -> do
          respond "Upgraded!"
          go h code' state
        Just (Do input) -> do
          let (output, _, state', _) = exec (input, code, state, [])
          respond (show output)
          go h code state'

respond :: String -> IO ()
respond s =
  withFile (pipePath <.> "response") WriteMode $ \h -> do
    hSetBuffering h LineBuffering
    hPutStrLn h s

tick :: IO ()
tick = writeFile (pipePath <.> "command") (show (Do (L Unit)) ++ "\n")

cget :: IO ()
cget = writeFile (pipePath <.> "command") (show (Do (R Unit)) ++ "\n")

load :: Code -> IO ()
load code = writeFile (pipePath <.> "command") (show (Load code) ++ "\n")

safeCreateNamedPipe :: FilePath -> IO ()
safeCreateNamedPipe fp =
  catchJust
    (\e -> if isAlreadyExistsErrorType (ioeGetErrorType e)
           then Just ()
           else Nothing)
    (createNamedPipe fp
      (namedPipeMode `unionFileModes`
       ownerReadMode `unionFileModes`
       ownerWriteMode))
    return
