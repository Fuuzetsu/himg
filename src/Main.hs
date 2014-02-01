{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      :  Main
-- Copyright   :  (c) Mateusz Kowalczyk 2013
-- License     :  GPL-3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
-- Portability :  portable
module Main where

import           Control.Exception
import qualified Data.ByteString.Lazy as L
import           Graphics.HImg (displayImage)
import           Graphics.UI.Gtk
import           Network.HTTP.Conduit
import           Network.URI (isURI)
import           System.Environment (getArgs)
import           System.Exit
import           System.Directory (doesFileExist)
import           System.IO
import           System.IO.Temp
import           Text.Printf (printf)

main ∷ IO ()
main = do
  getArgs >>= \case
    "-h":_ → printHelp >> exitWith (ExitFailure 1)
    ["-f", link] → viewFromFile link
    ["-u", link] → viewFromLink link
    [link] → viewGuess link
    _ → printHelp >> exitWith (ExitFailure 1)

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
  True → imageNewFromFile p >>= displayImage
  False → printf "%s doesn't exist\n" p >> exitWith (ExitFailure 2)

-- | Opens a passed in URL if possible. Exits with 2 if a download
-- problem occurs.
viewFromLink ∷ String → IO ()
viewFromLink link =
  if not $ isURI link
   then printf "%s is not a valid link\n" link >> exitWith (ExitFailure 2)
   else simpleHttp link >>= \bs → do
          withSystemTempFile "himg.imgfile" $ \p h →
            L.hPut h bs >> hClose h >> imageNewFromFile p >>= displayImage
        `catch`
        \(e ∷ HttpException) →
          printf "Encountered a problem while downloading %s: %s\n"
            link (show e)
          >> exitWith (ExitFailure 2)

-- | Attempts to guess whether the input is a file or a URL and opens the image
-- appropriately. Exits with 3 if it can't decide what it's looking at.
viewGuess ∷ String → IO ()
viewGuess p = doesFileExist p >>= \case
  True → viewFromFile p
  False → if isURI p
          then viewFromLink p
          else printf "Don't know how to open %s\n" p >> exitWith (ExitFailure 3)
