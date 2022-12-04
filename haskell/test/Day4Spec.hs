{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module Day4Spec (spec) where

import Control.Monad (forM_)
import Day4
  ( contains,
    intoPair,
    intoPairs,
    overlaps,
    solve1,
    solve2,
  )
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck ()
import Text.Printf (printf)
import Text.RawString.QQ (r)

data TestData a b = TestData {datum :: a, expectation :: b}

dataExample =
  [r|2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8|]

dataPair =
  [ TestData {datum = "2-4", expectation = (2, 4)},
    TestData {datum = "6-8", expectation = (6, 8)}
  ]

dataPairs =
  [ TestData {datum = "2-4,6-8", expectation = ((2, 4), (6, 8))},
    TestData {datum = "2-3,4-5", expectation = ((2, 3), (4, 5))}
  ]

dataContains =
  [ TestData {datum = ((2, 8), (3, 7)), expectation = True},
    TestData {datum = ((6, 6), (4, 6)), expectation = True},
    TestData {datum = ((2, 6), (4, 8)), expectation = False}
  ]

dataOverlaps =
  [ TestData {datum = ((2, 8), (3, 7)), expectation = True},
    TestData {datum = ((6, 6), (4, 6)), expectation = True},
    TestData {datum = ((2, 6), (4, 8)), expectation = True},
    TestData {datum = ((2, 6), (7, 10)), expectation = False}
  ]

spec :: Spec
spec = do
  describe "intoPair" $ do
    forM_ dataPair $ \d ->
      it "parses input pair string into a tuple" $ do
        intoPair d.datum `shouldBe` d.expectation

  describe "intoPairs" $ do
    forM_ dataPairs $ \d ->
      it "parses input pair strings into tuples" $ do
        intoPairs d.datum `shouldBe` d.expectation

  describe "contains" $ do
    forM_ dataContains $ \d ->
      it (printf "detects contained assignments for %s" (show d.datum)) $ do
        contains d.datum `shouldBe` d.expectation

  describe "overlaps" $ do
    forM_ dataOverlaps $ \d ->
      it (printf "detects overlapped assignments for %s" (show d.datum)) $ do
        overlaps d.datum `shouldBe` d.expectation

  describe "solve1" $ do
    it "solves all provides tests for part 1" $ do
      solve1 (map intoPairs (lines dataExample)) `shouldBe` 2

  describe "solve2" $ do
    it "solves all provided tests for part 2" $ do
      solve2 (map intoPairs (lines dataExample)) `shouldBe` 4
