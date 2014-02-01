{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
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
    [link] → undefined link
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

viewFromFile ∷ FilePath → IO ()
viewFromFile p = doesFileExist p >>= \case
  True → imageNewFromFile p >>= displayImage
  False → printf "%s doesn't exist" p >> exitWith (ExitFailure 2)

viewFromLink ∷ String → IO ()
viewFromLink link =
  if not $ isURI link
   then printf "%s is not a valid link" link >> exitWith (ExitFailure 2)
   else simpleHttp link >>= \bs → do
          withSystemTempFile "himg.imgfile" $ \p h →
            L.hPut h bs >> hClose h >> imageNewFromFile p >>= displayImage
        `catch`
        \(e ∷ HttpException) →
          printf "Encountered a problem while downloading %s: %s" link (show e)
          >> exitWith (ExitFailure 2)
