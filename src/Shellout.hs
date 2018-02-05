{-# LANGUAGE ScopedTypeVariables #-}
{-| Shell is a threaded manager that can run external processes
    and call functions of the `Driver` on process output/err/exit.
-}
module Shellout
(Driver(..), Shell, TaskName, Cmd, new)
where

import           Control.Concurrent       (MVar, newEmptyMVar, putMVar,
                                           takeMVar)
import           Control.Concurrent.Async (Async, async, link)
import           Control.Concurrent.STM   (TQueue, atomically, newTQueue,
                                           readTQueue, tryReadTQueue,
                                           writeTQueue)
import           Control.Monad            (unless)
import           Data.Text                (Text, unpack)
import           Data.Text.IO             (hGetLine)
import           GHC.IO.Handle            (Handle)
import           System.Exit              (ExitCode (..), exitWith)
import           System.IO                (hIsEOF)
import           System.Process.Typed     (closed, createPipe, getStderr,
                                           getStdout, setStderr, setStdin,
                                           setStdout, shell, waitExitCode,
                                           withProcess)

--------------------------------------------------------------------------------
-- TYPES

-- | Driver is a collection of functions that
-- describe what to do on process output
data Driver a = Driver

  { -- | (optionally) do something with the task name, and initialize
  -- data that will be passed between your handlers. you could store
  -- things like the task name, a spinner's position and last spin time, etc.
  initialState    :: Text -> a
  -- | what to do when polling returns `Nothing`, so it's waiting on
  -- more output from the task, but the task still hasn't exited.
  -- usually it's sleep.
  , handleNothing :: a -> IO a
  -- | what to do on stdout from the shell command. You could colorize it,
  -- append it to list of output (you could keep a list in the `a`), etc.
  , handleOut     :: a -> Text -> IO a
  -- | what to do on stderr. Same things go as stdout.
  , handleErr     :: a -> Text -> IO a
  -- | what to do when a task completes successfully
  , handleSuccess :: a -> IO ()
  -- | what to do when a task doesn't complete successfully
  , handleFailure :: a -> IO ()
  }

-- | The output of running an external process
data Output = Msg Text | Err Text | Success | Failure Int

-- | Processor has an input channel (for sending commands)
-- and an output channel (for reading the output)
data Processor = Processor (TQueue Text) (TQueue Output)

-- | Shell takes a task name and an external command and
-- executes the given callbacks in the provided driver
type Shell = (TaskName -> Cmd -> IO ())

-- | Task is the description of an external process
type TaskName = Text

-- | Cmd is the external command like 'cat file.txt' to run
type Cmd = Text

--------------------------------------------------------------------------------
-- API

-- | creates a new processor to run external processes in,
-- spawns a thread to run the processor loop, then returns
-- a `Shell` that can send commands to the processor loop,
-- and react to them with functions from the `Driver`
new :: Driver a -> IO Shell
new driver = do
  processor <- Processor <$> newChan <*> newChan
  _         <- spawn $ processorLoop processor
  pure $ execute processor driver
  where newChan = atomically newTQueue

--------------------------------------------------------------------------------
-- EXECUTE / OUTPUT LOOP

-- | executes the given command in the processor
execute :: forall a. Processor -> Driver a -> TaskName -> Cmd -> IO ()
execute (Processor input output) driver task cmd = do
  send input cmd                               -- send the command to the Processor thread
  loop (initialState driver task)              -- start the output loop
  where
    -- | try to read from the channel, returning Nothing if no value available
    maybeReceive = atomically . tryReadTQueue

    loop :: a -> IO ()
    loop acc = do
      out <- maybeReceive output
      case out of
        Nothing -> do
          newAcc <- handleNothing driver acc
          loop newAcc
        Just (Msg msg) -> do
          newAcc <- handleOut driver acc msg
          loop newAcc
        Just (Err msg) -> do
          newAcc <- handleErr driver acc msg
          loop newAcc
        Just Success ->
          handleSuccess driver acc
        Just (Failure c) -> do
          handleFailure driver acc
          exitWith $ ExitFailure c

--------------------------------------------------------------------------------
-- | PROCESSOR LOOP

-- | run the command, putting any stdout, stderr, and exits into the output channel.
-- will wait until stdout and stderr are empty to write the exit code.
processorLoop :: Processor -> IO ()
processorLoop processor@(Processor input output) = do
  cmd <- atomically $ readTQueue input -- receive input

  let config = setStdin closed
             $ setStdout createPipe
             $ setStderr createPipe
             $ shell (unpack cmd)

  withProcess config $ \p -> do
    stdoutLock <- newEmptyMVar -- create locks for stdout/stderr
    stderrLock <- newEmptyMVar

    _ <- spawn $ sendOutput Msg (getStdout p) stdoutLock
    _ <- spawn $ sendOutput Err (getStderr p) stderrLock

    code <- waitExitCode p -- wait for the exit and output locks
    takeMVar stdoutLock
    takeMVar stderrLock

    let result = case code of
          ExitSuccess   -> Success
          ExitFailure i -> Failure i

    send output result

  processorLoop processor

  where
    -- | read from the handle until it's empty, writing the result
    -- (wrapped in the given wrapper type) to the output channel
    -- and then releasing the given lock
    sendOutput :: (Text -> Output) -> Handle -> MVar () -> IO ()
    sendOutput wrap handle lock = do
      let loop = do
            isDone <- hIsEOF handle
            unless isDone $ do
              out <- hGetLine handle
              send output $ wrap out
              loop
      loop
      putMVar lock () -- release the lock

--------------------------------------------------------------------------------
-- THREAD AND CHANNEL HELPERS

-- | new async thread, linked to calling thread (will rethrow exceptions on linked thread)
spawn :: IO a -> IO (Async a)
spawn x = do
  thread <- async x
  link thread
  pure thread

-- | write to the channel
send :: TQueue a -> a -> IO ()
send x = atomically . writeTQueue x
