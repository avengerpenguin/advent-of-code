module Main where

import Control.Monad
import Lib
import System.IO

main = do
  contents <- readFile "input"
  print $ sumMatches . findMatches . join . lines $ contents
  print $ sumMatches . findEnabledMatches . join . lines $ contents
