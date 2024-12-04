module Main where

import Test.Hspec
import Compare

main :: IO ()
main = hspec $ do
    describe "tuple2" $ do
        it "converts list of 2 to 2-tuple" $ do
            tuple2 [1, 2] `shouldBe` (1, 2)
        it "converts list of 2+ to 2-tuple" $ do
            tuple2 [1, 2, 3, 4] `shouldBe` (1, 2)

    describe "parseRow" $ do
        it "parses 2 numbers" $ do
            parseRow "1   2" `shouldBe` (1, 2)

    describe "parseLists" $ do
        it "parses lists" $ do
            parseLists "1   2\n\
                       \3   4\n\
                       \5   6\n" `shouldBe` (
                          [1, 3, 5]
                        , [2, 4, 6]
                       )

    describe "distance" $ do
        it "calculates distance" $ do
            distance (
                [3, 6, 18]
              , [4, 9, 13]
              ) `shouldBe` 9

    describe "compareInput" $ do
        it "compares lists" $ do
            compareInput "3   4\n\
                         \4   3\n\
                         \2   5\n\
                         \1   3\n\
                         \3   9\n\
                         \3   3\n" `shouldBe` 11
