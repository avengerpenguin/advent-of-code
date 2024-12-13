module Main where

import Control.Monad
import Lib
import System.IO

main = do
  contents <- readFile "input"
  print $ countXmas $ contents
  print $ countCrossMas $ contents
