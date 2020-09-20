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
  ( inhibit,
    uninhibit,
    activate,
  )
where

import System.ScreenSaver.Inhibit.Process (exec)

-- | Inhibit the screensaver.
--
-- @since 0.0.0.0
inhibit :: IO ()
inhibit = void (exec "xset" ["s", "off"])

-- | Release the screensaver inhibit.
--
-- @since 0.0.0.0
uninhibit :: IO ()
uninhibit = void (exec "xset" ["s", "default"])

-- | Activate the screensaver.
--
-- @since 0.0.0.0
activate :: IO ()
activate = void (exec "xset" ["s", "activate"])
