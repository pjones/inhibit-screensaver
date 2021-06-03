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
module Main
  ( main,
  )
where

import Control.Concurrent (myThreadId, threadDelay, throwTo)
import qualified Options.Applicative as O
import System.Exit (ExitCode (..))
import qualified System.ScreenSaver.Inhibit.DBus as DBus
import qualified System.ScreenSaver.Inhibit.LoginCtl as LoginCtl
import qualified System.ScreenSaver.Inhibit.Process as Proc
import qualified System.ScreenSaver.Inhibit.XSet as XSet
import qualified System.Signal as Signal

-- | Command line options.
data Options = Options
  { -- | How often to run the script, in seconds.
    optionsFreq :: !Int,
    -- | Activate the screensaver if command fails.
    optionsActivate :: !Bool,
    -- | Script to run to test if the screensaver is currently active.
    optionsActiveTest :: !(Maybe String),
    -- | The command to execute.
    optionsExec :: !String,
    -- | Arguments to pass to the command.
    optionsArgs :: ![String]
  }

-- | Command line option parser.
options :: O.Parser Options
options =
  Options
    <$> O.option
      O.auto
      ( mconcat
          [ O.long "frequency",
            O.short 'f',
            O.metavar "SEC",
            O.value 300,
            O.showDefault,
            O.help "Run CMD every SEC seconds"
          ]
      )
    <*> O.switch
      ( mconcat
          [ O.long "activate",
            O.short 'a',
            O.help "Activate the screensaver if CMD fails"
          ]
      )
    <*> optional
      ( O.strOption $
          mconcat
            [ O.long "query",
              O.short 'q',
              O.metavar "SCRIPT",
              O.help "Run SCRIPT to test if screensaver is currently active"
            ]
      )
    <*> O.strArgument
      ( mconcat
          [ O.metavar "CMD",
            O.help "The command to execute"
          ]
      )
    <*> many
      ( O.strArgument $
          mconcat
            [ O.metavar "ARG",
              O.help "Arguments to pass to CMD"
            ]
      )

-- | Return 'True' if the screensaver is currently active.
screensaverIsActive :: Options -> IO Bool
screensaverIsActive Options {..} = case optionsActiveTest of
  Nothing -> pure False
  Just script -> Proc.exec (Just optionsFreq) "sh" ["-c", script]

-- | Main.
main :: IO ()
main = do
  options <-
    O.execParser
      ( O.info (options O.<**> O.helper) $
          mconcat
            [ O.fullDesc,
              O.progDesc "Inhibit the screensaver if a command exits cleanly."
            ]
      )

  tid <- myThreadId
  Signal.installHandler Signal.sigTERM (const $ throwTo tid ExitSuccess)

  hSetBuffering stdout LineBuffering

  DBus.withDBus $ \dbus ->
    XSet.withXSet (go options dbus)
  where
    go :: Options -> DBus.Client -> XSet.Client -> IO ()
    go options@Options {..} dbus xset = forever $ do
      putTextLn ("sleeping for " <> show optionsFreq <> " sec(s)")
      threadDelay (optionsFreq * 1000000)
      active <- screensaverIsActive options
      if active
        then uninhibit dbus xset
        else test options dbus xset

    test :: Options -> DBus.Client -> XSet.Client -> IO ()
    test Options {..} dbus xset =
      Proc.exec (Just optionsFreq) optionsExec optionsArgs >>= \case
        True -> do
          let reason = "command " <> optionsExec <> " exited with success"
          XSet.inhibit xset
          DBus.inhibit reason dbus
        False -> do
          uninhibit dbus xset
          when optionsActivate $ do
            XSet.activate xset
            LoginCtl.activate

    uninhibit :: DBus.Client -> XSet.Client -> IO ()
    uninhibit dbus xset = do
      XSet.uninhibit xset
      DBus.uninhibit dbus
