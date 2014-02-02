{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LambdaCase #-}
-- |
-- Module      :  Main
-- Copyright   :  (c) Mateusz Kowalczyk 2014
-- License     :  GPL-3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
-- Portability :  portable
module Main where

import           Graphics.HImg
import           System.Environment (getArgs)
import           System.Exit        (exitWith, ExitCode(ExitFailure))

main ∷ IO ()
main = getArgs >>= \case
  "-h":_       → printHelp >> exitWith (ExitFailure 1)
  ["-f", link] → viewFromFile link
  ["-u", link] → viewFromLink link
  [link]       → viewGuess link
  _            → printHelp >> exitWith (ExitFailure 1)
