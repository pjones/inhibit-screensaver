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
module System.ScreenSaver.Inhibit.XSet
  ( Client,
    withXSet,
    inhibit,
    uninhibit,
    activate,
  )
where

import System.ScreenSaver.Inhibit.Process (exec)

-- | Internal data.
--
-- @since 0.0.0.0
newtype Client = Client (IORef Bool)

-- | Run a computation that expects a 'Client' value.
--
-- @since 0.0.0.0
withXSet :: (Client -> IO a) -> IO a
withXSet f = do
  ref <- newIORef False
  f (Client ref)

-- | Inhibit the screensaver.
--
-- @since 0.0.0.0
inhibit :: Client -> IO ()
inhibit (Client ref) =
  unlessM (readIORef ref) $ do
    void (exec (Just 30) "xset" ["s", "off"])
    writeIORef ref True

-- | Release the screensaver inhibit.
--
-- @since 0.0.0.0
uninhibit :: Client -> IO ()
uninhibit (Client ref) =
  whenM (readIORef ref) $ do
    void (exec (Just 30) "xset" ["s", "default"])
    writeIORef ref False

-- | Activate the screensaver.
--
-- @since 0.0.0.0
activate :: Client -> IO ()
activate client = do
  uninhibit client
  void (exec (Just 30) "xset" ["s", "activate"])
