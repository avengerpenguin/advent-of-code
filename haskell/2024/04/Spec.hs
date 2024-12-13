module Main where

import Lib
import Test.Hspec

testGrid =
  "MMMSXXMASM\n\
 \MSAMXMSMSA\n\
 \AMXSXMAAMM\n\
 \MSAMASMSMX\n\
 \XMASAMXAMM\n\
 \XXAMMXXAMA\n\
 \SMSMSASXSS\n\
 \SAXAMASAAA\n\
 \MAMMMXMMMM\n\
 \MXMXAXMASX"

smolGrid =
  "MMMS\n\
 \MSAM\n\
 \AMXS\n\
 \MSAM"

main :: IO ()
main =
  hspec $ do
    describe "rows" $ do
      it "finds rows" $ do
        rows testGrid
          `shouldBe` [ "MMMSXXMASM"
                     , "MSAMXMSMSA"
                     , "AMXSXMAAMM"
                     , "MSAMASMSMX"
                     , "XMASAMXAMM"
                     , "XXAMMXXAMA"
                     , "SMSMSASXSS"
                     , "SAXAMASAAA"
                     , "MAMMMXMMMM"
                     , "MXMXAXMASX"
                     ]
    describe "cols" $ do
      it "finds cols" $ do
        cols testGrid
          `shouldBe` [ "MMAMXXSSMM"
                     , "MSMSMXMAAX"
                     , "MAXAAASXMM"
                     , "SMSMSMMAMX"
                     , "XXXAAMSMMA"
                     , "XMMSMXAAXX"
                     , "MSAMXXSSMM"
                     , "AMASAAXAMA"
                     , "SSMMMMSAMS"
                     , "MAMXMASAMX"
                     ]
    describe "diagLeft" $ do
      it "finds left to right diagonals" $ do
        diagLeft testGrid
          `shouldBe` [ "MSXMAXSAMX"
                     , "MASAMXXAM"
                     , "MMXSXASA"
                     , "SXMMAMS"
                     , "XMASMA"
                     , "XSAMM"
                     , "MMMX"
                     , "ASM"
                     , "SA"
                     , "M"
                     , "XMASXXSMA"
                     , "MMMAXAMM"
                     , "XMASAMX"
                     , "AXSXMM"
                     , "XMASA"
                     , "MMAS"
                     , "AMA"
                     , "SM"
                     , "X"
                     ]
    describe "diagRight" $ do
      it "finds right to left diagonals" $ do
        diagRight testGrid
          `shouldBe` [ "MSAMMMMXAM"
                     , "SMASAMSAM"
                     , "ASMASAMS"
                     , "MMXMAXS"
                     , "XXSAMX"
                     , "XMXSX"
                     , "SAMM"
                     , "MSA"
                     , "MM"
                     , "M"
                     , "SMSAMSAMM"
                     , "AMASMASA"
                     , "MXMMAMM"
                     , "XMASXX"
                     , "AMXMX"
                     , "XMAS"
                     , "MAS"
                     , "XM"
                     , "M"
                     ]
    describe "countXmas" $ do
      it "counts XMAS" $ do
        countXmas testGrid `shouldBe` 18
    describe "grid" $ do
      it "grids" $ do
        grid testGrid
          `shouldBe` [ ["M", "M", "M", "S", "X", "X", "M", "A", "S", "M"]
                     , ["M", "S", "A", "M", "X", "M", "S", "M", "S", "A"]
                     , ["A", "M", "X", "S", "X", "M", "A", "A", "M", "M"]
                     , ["M", "S", "A", "M", "A", "S", "M", "S", "M", "X"]
                     , ["X", "M", "A", "S", "A", "M", "X", "A", "M", "M"]
                     , ["X", "X", "A", "M", "M", "X", "X", "A", "M", "A"]
                     , ["S", "M", "S", "M", "S", "A", "S", "X", "S", "S"]
                     , ["S", "A", "X", "A", "M", "A", "S", "A", "A", "A"]
                     , ["M", "A", "M", "M", "M", "X", "M", "M", "M", "M"]
                     , ["M", "X", "M", "X", "A", "X", "M", "A", "S", "X"]
                     ]
    describe "windows" $ do
      it "windows" $ do
        windows smolGrid
          `shouldBe` ["MMMMSAAMX", "MMSSAMMXS", "MSAAMXMSA", "SAMMXSSAM"]
--    describe "countCrossMas" $ do
--        it "counts X-MAS" $ do
--            countCrossMas (grid testGrid) `shouldBe` 9
