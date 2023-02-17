{-# LANGUAGE ExistentialQuantification #-}

module LibMain where

import System.IO
import System.FilePath
import System.Posix.Files
import Control.Exception
import Text.Read
import System.IO.Error

import Code
import AbstractMachine
import StateMachine
import Compiler

------------------------------------------------------------------------

data Command = Do Value | Load Code Code Code
  deriving (Show, Read)

pipePath :: FilePath
pipePath = "/tmp/freefunc"

libMain :: Code -> IO ()
libMain code0 = do
  safeCreateNamedPipe (pipePath <.> "command")
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
        Just (Load old new migration) -> do
          if code /= old
          then do
            putStrLn "The version running isn't the one the upgrade expects. Aborting upgrade."
            go h code state
          else do
            let (migratedState, _, _, _) = exec (state, migration, Unit, [])
            putStrLn "Upgrade successful!"
            go h new migratedState
        Just (Do input) -> do
          let (output, _, state', _) = exec (input, code, state, [])
          putStrLn ("Output:    " ++ show output)
          putStrLn ("New state: " ++ show state')
          go h code state'

incr :: IO ()
incr = writeFile (pipePath <.> "command") (show (Do (L Unit)) ++ "\n")

count :: IO ()
count = writeFile (pipePath <.> "command") (show (Do (R Unit)) ++ "\n")

data Upgrade = forall s s' a a' b b'. Upgrade
  { oldSM          :: FreeFunc s a b
  , newSM          :: FreeFunc s' a' b'
  , stateMigration :: FreeFunc () s s'
  }

upgrade :: Upgrade -> IO ()
upgrade (Upgrade old new migration) = writeFile (pipePath <.> "command")
  (show (Load (compile old) (compile new) (compile migration)) ++ "\n")

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
