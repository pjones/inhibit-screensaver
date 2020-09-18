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

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import qualified DBus as DBus
import qualified DBus.Client as DBus
import qualified Options.Applicative as O
import System.Exit (ExitCode (..))
import qualified System.Process as Proc

-- | Command line options.
data Options = Options
  { -- | How often to run the script, in seconds.
    optionsFreq :: Int,
    -- | The command to execute.
    optionsExec :: String,
    -- | Arguments to pass to the command.
    optionsArgs :: [String]
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

-- | Type for tacking the inhibit cookie.
type Cookie = IORef (Maybe Word32)

-- | Execute the command and return 'True' if the screensaver should
-- be inhibited.
exec :: Options -> IO Bool
exec Options {..} = do
  putTextLn ("starting process: " <> toText optionsExec)
  (_, _, _, process) <-
    Proc.proc optionsExec optionsArgs
      & Proc.createProcess
  Proc.waitForProcess process >>= \case
    ExitSuccess -> do
      putTextLn "process exited with success"
      pure True
    ExitFailure e -> do
      putTextLn ("process existed with failure code " <> show e)
      pure False

-- | Prepare a DBus method call.
mkMethod :: DBus.MemberName -> DBus.MethodCall
mkMethod name =
  ( DBus.methodCall
      "/org/freedesktop/ScreenSaver"
      "org.freedesktop.ScreenSaver"
      name
  )
    { DBus.methodCallDestination =
        Just "org.freedesktop.ScreenSaver"
    }

-- | Inhibit the screensaver if it's not already inhibited.
inhibit :: Options -> Cookie -> DBus.Client -> IO ()
inhibit Options {..} cookie client =
  readIORef cookie >>= \case
    Just _ -> pass
    Nothing -> do
      let method =
            (mkMethod "Inhibit")
              { DBus.methodCallBody =
                  [ DBus.toVariant appName,
                    DBus.toVariant reason
                  ]
              }
      putTextLn "requesting inhibit cookie"
      reply <- DBus.call_ client method
      case listToMaybe (DBus.methodReturnBody reply) >>= DBus.fromVariant of
        Nothing -> putTextLn "error: failed to acquire inhibit cookie"
        Just c -> do
          putTextLn ("acquired inhibit cookie " <> show c)
          writeIORef cookie (Just c)
  where
    appName :: String
    appName = "com.github.pjones.inhibit-screensaver"

    reason :: String
    reason = "process " <> optionsExec <> " exited with success"

-- | Release the inhibit cookie if one is currently held.
uninhibit :: Cookie -> DBus.Client -> IO ()
uninhibit cookie client =
  readIORef cookie >>= \case
    Nothing -> pass
    Just c -> do
      let method =
            (mkMethod "UnInhibit")
              { DBus.methodCallBody =
                  [ DBus.toVariant c
                  ]
              }
      putTextLn "releasing inhibit cookie"
      _ <- DBus.call_ client method
      pass

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

  cookie <- newIORef Nothing

  bracket
    DBus.connectSession
    (\c -> uninhibit cookie c >> DBus.disconnect c)
    (go options cookie)
  where
    go :: Options -> Cookie -> DBus.Client -> IO ()
    go options cookie client = forever $ do
      test <- exec options
      bool (uninhibit cookie client) (inhibit options cookie client) test
      threadDelay (optionsFreq options * 1000000)
