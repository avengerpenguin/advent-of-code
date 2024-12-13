module Main where

import Compare
import System.IO

main = do
  contents <- readFile "input"
  print $ compareDistance contents
  print $ calculateSimilarityFromString contents
