{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      :  Graphics.HImg
-- Copyright   :  (c) Mateusz Kowalczyk 2014
-- License     :  GPL-3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
-- Portability :  portable
module Graphics.HImg ( displayImage
                     , printHelp
                     , viewFromFile
                     , viewFromLink
                     , viewGuess
                     ) where

import           Control.Exception          (catch)
import           Data.ByteString.Lazy       (hPut)
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.Gdk.Events (Event(..))
import           Network.HTTP.Conduit       (simpleHttp, HttpException)
import           Network.URI                (isURI)
import           System.Directory           (doesFileExist)
import           System.Exit                (exitWith, ExitCode(ExitFailure))
import           System.IO                  (hClose)
import           System.IO.Temp             (withSystemTempFile)
import           Text.Printf                (printf)

-- | Takes 'Image' and spawns a window which displays it. It does not do any
-- scaling and such. You can kill the window with ‘q’.
displayImage ∷ Image → IO ()
displayImage img = do
  _ ← initGUI
  window ← windowNew

  set window [ containerBorderWidth := 0
             , containerChild       := img
             , windowResizable      := False
             ]

  mapM_ ($ window) [ (`onKeyPress` keyPressedEvent)
                   , (`onDestroy` mainQuit)
                   ]

  widgetShowAll window
  mainGUI

-- | Handles key events for the window spawned by 'displayImage'.
keyPressedEvent ∷ Event → IO Bool
keyPressedEvent Key{eventKeyChar = Just 'q'} = mainQuit >> return True
keyPressedEvent _                            = return False

-- | Prints the help message.
printHelp ∷ IO ()
printHelp = putStr . unlines $
  [ "HImg: very simple image viewer"
  , ""
  , "-h          Prints this help."
  , "-f <file>   Opens the specified file."
  , "-u <url>    Opens the file from the specified url."
  , "<link>      Tries to guess whether something's a file or url and opens it."
  , ""
  , "Example usage:"
  , "himg -f /tmp/foo.jpg"
  , "himg http://example.com/animation.gif"
  , ""
  , "To quit the viewer, press ‘q’."
  ]

-- | Opens 'FilePath' from a file. Exits with 2 if the file doesn't exist.
viewFromFile ∷ FilePath → IO ()
viewFromFile p = doesFileExist p >>= \case
  True  → imageNewFromFile p >>= displayImage
  False → printf "%s doesn't exist\n" p >> exitWith (ExitFailure 2)

-- | Opens a passed in URL if possible. Exits with 2 if a download
-- problem occurs.
viewFromLink ∷ String → IO ()
viewFromLink link =
  if not $ isURI link
   then printf "%s is not a valid link\n" link >> exitWith (ExitFailure 2)
   else simpleHttp link >>= \bs → do
          withSystemTempFile "himg.imgfile" $ \p h →
            hPut h bs >> hClose h >> imageNewFromFile p >>= displayImage
        `catch`
        \(e ∷ HttpException) →
          printf "Encountered a problem while downloading %s: %s\n"
            link (show e)
          >> exitWith (ExitFailure 2)

-- | Attempts to guess whether the input is a file or a URL and opens the image
-- appropriately. Exits with 3 if it can't decide what it's looking at.
viewGuess ∷ String → IO ()
viewGuess p = doesFileExist p >>= \case
  True  → viewFromFile p
  False → if isURI p
           then viewFromLink p
           else printf "Don't know how to open %s\n" p
                >> exitWith (ExitFailure 3)
