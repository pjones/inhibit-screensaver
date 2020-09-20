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
import qualified System.Process as Proc

-- | Execute the command and return 'True' if the screensaver should
-- be inhibited.
--
-- @since 0.0.0.0
exec :: String -> [String] -> IO Bool
exec name args = catch go handle
  where
    go :: IO Bool
    go = do
      putTextLn ("starting process: " <> toText (intercalate " " $ name : args))
      (_, _, _, process) <- Proc.proc name args & Proc.createProcess
      Proc.waitForProcess process >>= \case
        ExitSuccess -> do
          putTextLn "process exited with success"
          pure True
        ExitFailure e -> do
          putTextLn ("process existed with failure code " <> show e)
          pure False

    handle :: IOException -> IO Bool
    handle e = do
      putTextLn ("process failed to start: " <> show e)
      pure False