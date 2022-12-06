{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module Day6Spec (spec) where

import Control.Monad (forM_)
import Day6
  ( solve1,
    solve2,
  )
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck ()
import Text.Printf (printf)
import Text.RawString.QQ (r)

data TestData a b = TestData {datum :: a, expectation :: b}

testSignals =
  [ "mjqjpqmgbljsphdztnvjfqwrcgsmlb",
    "bvwbjplbgvbhsrlpgdmjqwftvncz",
    "nppdvjthqldpwncqszvftbrmjlhg",
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
  ]

testSignals4 =
  [ TestData {datum = testSignals !! 0, expectation = 7},
    TestData {datum = testSignals !! 1, expectation = 5},
    TestData {datum = testSignals !! 2, expectation = 6},
    TestData {datum = testSignals !! 3, expectation = 10},
    TestData {datum = testSignals !! 4, expectation = 11}
  ]

testSignals14 =
  [ TestData {datum = testSignals !! 0, expectation = 19},
    TestData {datum = testSignals !! 1, expectation = 23},
    TestData {datum = testSignals !! 2, expectation = 23},
    TestData {datum = testSignals !! 3, expectation = 29},
    TestData {datum = testSignals !! 4, expectation = 26}
  ]

spec :: Spec
spec = do
  describe "solve1" $ do
    forM_ testSignals4 $ \d ->
      it "solves data example provided by the exercise" $ do
        solve1 d.datum `shouldBe` d.expectation

  describe "solve2" $ do
    forM_ testSignals14 $ \d ->
      it "solves data example provided by the exercise" $ do
        solve2 d.datum `shouldBe` d.expectation
