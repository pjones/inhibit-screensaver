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
module System.ScreenSaver.Inhibit.DBus
  ( Client,
    withDBus,
    inhibit,
    uninhibit,
  )
where

import Control.Exception (bracket)
import qualified DBus as DBus
import qualified DBus.Client as DBus

-- | DBus connection details.
--
-- @since 0.0.0.0
data Client = Client
  { -- | The DBus connection.
    clientConnection :: !DBus.Client,
    -- | The assigned inhibit cookie.
    clientCookie :: !(IORef (Maybe Word32))
  }

-- | Create a client connection and pass it to the given function.
--
-- @since 0.0.0.0
withDBus :: (Client -> IO a) -> IO a
withDBus f =
  bracket
    (Client <$> DBus.connectSession <*> newIORef Nothing)
    (\c -> uninhibit c >> DBus.disconnect (clientConnection c))
    f

-- | Inhibit the screensaver if it's not already inhibited.
--
-- @since 0.0.0.0
inhibit :: String -> Client -> IO ()
inhibit reason Client {..} = do
  putTextLn "requesting screensaver inhibit"
  readIORef clientCookie >>= \case
    Just _ -> pass
    Nothing -> do
      let method =
            (mkMethod "Inhibit")
              { DBus.methodCallBody =
                  [ DBus.toVariant appName,
                    DBus.toVariant reason
                  ]
              }
      reply <- DBus.call_ clientConnection method
      case listToMaybe (DBus.methodReturnBody reply) >>= DBus.fromVariant of
        Nothing -> putTextLn "error: failed to acquire inhibit cookie"
        Just c -> do
          putTextLn ("acquired inhibit cookie " <> show c)
          writeIORef clientCookie (Just c)
  where
    appName :: String
    appName = "com.github.pjones.inhibit-screensaver"

-- | Release the inhibit cookie if one is currently held.
--
-- @since 0.0.0.0
uninhibit :: Client -> IO ()
uninhibit Client {..} = do
  putTextLn "releasing screensaver inhibit"
  readIORef clientCookie >>= \case
    Nothing -> pass
    Just c -> do
      let method =
            (mkMethod "UnInhibit")
              { DBus.methodCallBody =
                  [ DBus.toVariant c
                  ]
              }
      _ <- DBus.call_ clientConnection method
      pass

-- | Prepare a DBus method call.
mkMethod :: DBus.MemberName -> DBus.MethodCall
mkMethod name =
  ( DBus.methodCall
      "/ScreenSaver"
      "org.freedesktop.ScreenSaver"
      name
  )
    { DBus.methodCallDestination =
        Just "org.freedesktop.ScreenSaver"
    }
