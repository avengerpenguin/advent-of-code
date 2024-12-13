module Main where

import Lib
import System.IO

main = do
  contents <- readFile "input"
  print $ checkSafeFromLines $ lines contents
  print $ checkSafeFromLinesWithRemoval $ lines contents
