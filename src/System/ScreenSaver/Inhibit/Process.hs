-- |
--
-- Copyright:
--   This file is part of the package inhibit-screensaver. It is
--   subject to the license terms in the LICENSE file found in the
--   top-level directory of this distribution and at:
--
--     https://github.com/pjones/inhibit-screensaver
--
--   No part of this package, including this file, may be copied,
--   modified, propagated, or distributed except according to the terms
--   contained in the LICENSE file.
--
-- License: BSD-2-Clause
module System.ScreenSaver.Inhibit.Process
  ( exec,
  )
where

import Control.Exception (IOException, catch)
import System.Exit (ExitCode (..))
import System.IO (hFlush)
import qualified System.Process as Proc
import System.Timeout (timeout)

-- | Execute the command and return 'True' if the screensaver should
-- be inhibited.
--
-- @since 0.0.0.0
exec :: Maybe Int -> String -> [String] -> IO Bool
exec waitSec name args = catch go handle
  where
    go :: IO Bool
    go = do
      putTextLn ("starting process: " <> toText (intercalate " " $ name : args))
      hFlush stdout

      (_, _, _, process) <- Proc.proc name args & Proc.createProcess

      timeout
        (maybe (-1) (* 1000000) waitSec)
        (Proc.waitForProcess process)
        >>= \case
          Nothing -> do
            putTextLn "process timed out"
            pure False
          Just ExitSuccess -> do
            putTextLn "process exited with success"
            pure True
          Just (ExitFailure e) -> do
            putTextLn ("process existed with failure code " <> show e)
            pure False

    handle :: IOException -> IO Bool
    handle e = do
      putTextLn ("process failed to start: " <> show e)
      pure False
