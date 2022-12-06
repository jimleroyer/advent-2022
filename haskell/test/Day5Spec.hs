{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module Day5Spec (spec) where

import Control.Monad (forM_)
import Day5
  ( Step (..),
    intoStep,
    move9000,
    move9001,
    solve1,
    solve2,
  )
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck ()
import Text.Printf (printf)
import Text.RawString.QQ (r)

data TestData a b = TestData {datum :: a, expectation :: b}

dataExampleStacks = ["NZ", "DCM", "P"]

dataExampleSteps =
  [r|move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2|]

dataSteps =
  [ TestData {datum = "move 1 from 2 to 1", expectation = Step 2 1 1},
    TestData {datum = "move 3 from 1 to 3", expectation = Step 1 3 3},
    TestData {datum = "move 2 from 2 to 1", expectation = Step 2 1 2},
    TestData {datum = "move 1 from 1 to 2", expectation = Step 1 2 1}
  ]

spec :: Spec
spec = do
  describe "steps parsing" $ do
    forM_ dataSteps $ \d ->
      it (printf "parses steps '%s' into data structure" (d.datum)) $ do
        intoStep d.datum `shouldBe` d.expectation

  describe "move function" $ do
    it "should properly move the stack" $ do
      move9000 dataExampleStacks (Step {from = 2, to = 1, num = 1}) `shouldBe` ["DNZ", "CM", "P"]
      move9000 dataExampleStacks (Step {from = 2, to = 1, num = 2}) `shouldBe` ["CDNZ", "M", "P"]
      move9000 dataExampleStacks (Step {from = 2, to = 1, num = 3}) `shouldBe` ["MCDNZ", "", "P"]

      move9000 dataExampleStacks (Step {from = 2, to = 3, num = 3}) `shouldBe` ["NZ", "", "MCDP"]

      move9000 dataExampleStacks (Step {from = 1, to = 3, num = 0}) `shouldBe` ["NZ", "DCM", "P"]
      move9000 dataExampleStacks (Step {from = 1, to = 3, num = 2}) `shouldBe` ["", "DCM", "ZNP"]

  describe "solve1" $ do
    it "solves data example provided by the exercise" $ do
      solve1 (map intoStep (lines dataExampleSteps)) dataExampleStacks `shouldBe` "CMZ"

  describe "solve2" $ do
    it "solves data example provided by the exercise" $ do
      solve2 (map intoStep (lines dataExampleSteps)) dataExampleStacks `shouldBe` "MCD"
