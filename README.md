# shellout

A simple library for running standard processes in a threaded manager
, and responding to/doing things with stdout, stderr, and exits as they happen.

This was mainly built out of frustration that most libraries wait for a task
to complete before giving output.

Just initialize with a driver and off you go!

## Example

Here's a short annotated example on how to use this:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Shellout

import           Control.Concurrent (threadDelay)
import           Data.Monoid        ((<>))
import           Data.Text          (Text)
import qualified Data.Text.IO       as T

-- create a type to hold data that will be passed between all of your handlers,
-- where you could store things like the task name, a spinner's position
-- and last spin time, etc.
newtype Task = Task {name :: Text}

main :: IO ()
main = do
  -- create a 'driver', with functions that handle each type of output
  let driver = Shellout.Driver
        -- (optionally) do something with the task name, and initialize
        -- data that will be passed between your handlers. you could store
        -- things like the task name, a spinner's position and last spin time, etc.
        { Shellout.initialState =
          \taskName -> Task {name = taskName}

        -- what to do when polling returns `Nothing`, so it's waiting on
        -- more output from the task, but the task still hasn't exited.
        -- In this example, we just sleep.
        , Shellout.handleNothing =
          \task -> threadDelay 1000 >> pure task

        -- what to do on stdout from the shell command. You could colorize it,
        -- append it to list of output (you could keep a list of them in the `Task`), etc.
        , Shellout.handleOut =
          \task txt -> T.putStrLn (name task <> ": " <> txt) >> pure task

        -- what to do on stderr. Same things go as stdout.
        , Shellout.handleErr =
          \task txt -> T.putStrLn (name task <> ": " <> txt) >> pure task

        -- what to do when a task completes successfully
        , Shellout.handleSuccess =
          \task -> T.putStrLn $ (name task) <> " complete."

        -- what to do when a task doesn't complete successfully
        , Shellout.handleFailure =
          \task -> T.putStrLn $ (name task) <> " failed."
        }

  shell <- Shellout.new driver

  shell "listing directory" "ls"

  shell "listing hidden files" "ls -lah"
```