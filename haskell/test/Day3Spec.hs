{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module Day3Spec (spec) where

import Control.Monad (forM_)
import Day3
  ( halfSplit,
    mkUniq,
    part1,
    part2,
    priority,
    sameInBags,
    sameInCompartment,
    solve1,
    solve2,
  )
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck ()
import Text.RawString.QQ (r)

data TestData a b = TestData {datum :: a, expectation :: b}

dataBags =
  [ "vJrwpWtwJgWrhcsFMMfFFhFp",
    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
    "PmmdzqPrVvPwwTWBwg",
    "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
    "ttgJtRGJQctTZtZT",
    "CrZsJsPPZsGzwwsLwLmpwMDw"
  ]

dataGroups =
  [ [r|vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
|],
    [r|wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw|]
  ]

dataCompartments :: [TestData String String]
dataCompartments =
  [ TestData {datum = dataBags !! 0, expectation = "p"},
    TestData {datum = dataBags !! 1, expectation = "L"},
    TestData {datum = dataBags !! 2, expectation = "P"},
    TestData {datum = dataBags !! 3, expectation = "v"},
    TestData {datum = dataBags !! 4, expectation = "t"},
    TestData {datum = dataBags !! 5, expectation = "s"}
  ]

dataBadges :: [TestData String String]
dataBadges =
  [ TestData {datum = dataGroups !! 0, expectation = "r"},
    TestData {datum = dataGroups !! 1, expectation = "Z"}
  ]

dataSolve1 :: [TestData String Int]
dataSolve1 =
  [ TestData {datum = dataBags !! 0, expectation = 16},
    TestData {datum = dataBags !! 1, expectation = 38},
    TestData {datum = dataBags !! 2, expectation = 42},
    TestData {datum = dataBags !! 3, expectation = 22},
    TestData {datum = dataBags !! 4, expectation = 20},
    TestData {datum = dataBags !! 5, expectation = 19}
  ]

dataSolve2 :: [TestData String Int]
dataSolve2 =
  [ TestData {datum = dataGroups !! 0, expectation = 18},
    TestData {datum = dataGroups !! 1, expectation = 52}
  ]

spec :: Spec
spec = do
  describe "priority" $ do
    it "returns 1 for 'a'" $ do
      priority 'a' `shouldBe` 1

    it "returns 27 for 'A'" $ do
      priority 'A' `shouldBe` 27

  describe "halfSplit" $ do
    it "splits a list in half" $ do
      halfSplit [1, 2, 3, 4, 5, 6] `shouldBe` ([1, 2, 3], [4, 5, 6])

  describe "mkUniq" $ do
    it "removes duplicates out of list" $ do
      mkUniq [1, 2, 3, 3, 4, 5, 5, 6] `shouldBe` [1, 2, 3, 4, 5, 6]

  describe "sameInCompartment" $ do
    forM_ dataCompartments $ \d ->
      it "returns same items contained in both bag's compartments" $ do
        sameInCompartment d.datum `shouldBe` d.expectation

  describe "sameInBags" $ do
    forM_ dataBadges $ \d ->
      it "returns same items contained in all bags" $ do
        sameInBags (lines d.datum) `shouldBe` d.expectation

  describe "solve1" $ do
    forM_ dataSolve1 $ \d ->
      it "solves all provides tests for part 1" $ do
        solve1 d.datum `shouldBe` d.expectation

  describe "solve2" $ do
    forM_ dataSolve2 $ \d ->
      it "solves all provided tests for part 2" $ do
        solve2 (lines d.datum) `shouldBe` d.expectation
