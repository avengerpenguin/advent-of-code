module Main where

import Test.Hspec
import Lib
import qualified Data.Map.Strict as Map

main :: IO ()
main = hspec $ do
    describe "checkSafe" $ do
        it "says example 1 is safe" $ do
            checkSafe [7, 6, 4, 2, 1] `shouldBe` True

    describe "checkSafeFromString" $ do
        it "says example 1 is safe" $ do
            checkSafeFromString "7 6 4 2 1" `shouldBe` True
        it "says example 2 is unsafe" $ do
            checkSafeFromString "1 2 7 8 9" `shouldBe` False
        it "says example 3 is unsafe" $ do
            checkSafeFromString "9 7 6 2 1" `shouldBe` False
        it "says example 4 is unsafe" $ do
            checkSafeFromString "1 3 2 4 5" `shouldBe` False
        it "says example 5 is unsafe" $ do
            checkSafeFromString "8 6 4 4 1" `shouldBe` False
        it "says example 6 is safe" $ do
            checkSafeFromString "1 3 6 7 9" `shouldBe` True

    describe "checkSafeFromLines" $ do
        it "example has 2 safe as expected" $ do
            checkSafeFromLines [
                   "7 6 4 2 1"
                 , "1 2 7 8 9"
                 , "9 7 6 2 1"
                 , "1 3 2 4 5"
                 , "8 6 4 4 1"
                 , "1 3 6 7 9"
             ] `shouldBe` 2
