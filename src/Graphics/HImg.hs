{-# LANGUAGE UnicodeSyntax #-}
module Graphics.HImg where

import Control.Concurrent
import System.Exit
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import Graphics.UI.Gtk.Gdk.Events

-- pimg = pixbufNewFromFile "/home/shana/images/414px-airi_suzuki_buono21.jpg"
pimg = pixbufNewFromFile "/tmp/HenNeko.gif"

displayImage ∷ Pixbuf → IO ()
displayImage b = do
  initGUI
  window ← windowNew
  drawingArea ← drawingAreaNew
  hbox ← hBoxNew True 0

  boxPackStart hbox drawingArea PackGrow 0
  set window [ containerBorderWidth := 0, containerChild := hbox ]

  -- We block main thread on this until something in the program
  -- decides that it's time to kill the main window and exit.
  exitVar ← newEmptyMVar

  onDestroy window $ putMVar exitVar ()
  onKeyPress window (keyPressedEvent exitVar)
  widgetShowAll window

  drawable ← widgetGetDrawWindow drawingArea
  gc ← gcNew drawable

  onExpose drawingArea (reloadImage b drawable gc)

  -- Fork the graphics stuff and block until user exits
  forkIO mainGUI
  takeMVar exitVar
  widgetDestroy window
  mainQuit


reloadImage ∷ Pixbuf → DrawWindow → GC → Event → IO Bool
reloadImage pix drawable gc _ = do
  drawPixbuf drawable gc pix 0 0 0 0 (-1) (-1) RgbDitherNone 0 0
  return True

keyPressedEvent ∷ MVar () → Event → IO Bool
keyPressedEvent m Key{eventKeyChar = Just 'q'} = putMVar m () >> return True
keyPressedEvent _ _ = return False
