{-# LANGUAGE UnicodeSyntax #-}
module Graphics.HImg (displayImage) where

import Control.Concurrent
import System.Exit
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import Graphics.UI.Gtk.Gdk.Events

-- | Takes 'Image' and spawns a window which displays it. It does not do any
-- scaling and such. You can kill the window with ‘q’.
displayImage ∷ Image → IO ()
displayImage img = do
  initGUI
  window ← windowNew

  set window [ containerBorderWidth := 0, containerChild := img
             , windowResizable := False ]

  -- We block main thread on this until something in the program
  -- decides that it's time to kill the main window and exit.
  exitVar ← newEmptyMVar

  mapM_ ($ window) [ (`onDestroy` putMVar exitVar ())
                   , (`onKeyPress` keyPressedEvent exitVar)
                   ]
  widgetShowAll window

  -- Fork the graphics stuff and block until user exits
  forkIO mainGUI
  takeMVar exitVar
  widgetDestroy window
  mainQuit

-- | Handles key events for the window spawned by 'displayImage'.
keyPressedEvent ∷ MVar () → Event → IO Bool
keyPressedEvent m Key{eventKeyChar = Just 'q'} = putMVar m () >> return True
keyPressedEvent _ _ = return False
