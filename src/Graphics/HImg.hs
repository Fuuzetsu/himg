{-# LANGUAGE UnicodeSyntax #-}
-- |
-- Module      :  Graphics.HImg
-- Copyright   :  (c) Mateusz Kowalczyk 2013
-- License     :  GPL-3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
-- Portability :  portable
module Graphics.HImg (displayImage) where

import Control.Concurrent
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events

-- | Takes 'Image' and spawns a window which displays it. It does not do any
-- scaling and such. You can kill the window with ‘q’.
displayImage ∷ Image → IO ()
displayImage img = do
  _ ← initGUI
  window ← windowNew

  set window [ containerBorderWidth := 0
             , containerChild := img
             , windowResizable := False
             ]

  -- We block main thread on this until something in the program
  -- decides that it's time to kill the main window and exit.

  mapM_ ($ window) [ (`onKeyPress` keyPressedEvent window)
                   , (`onDestroy` mainQuit)
                   ]

  widgetShowAll window

  -- Fork the graphics stuff and block until user exits
  mainGUI

-- | Handles key events for the window spawned by 'displayImage'.
keyPressedEvent ∷ Window → Event → IO Bool
keyPressedEvent w Key{eventKeyChar = Just 'q'} = mainQuit >> return True
keyPressedEvent _ _ = return False
