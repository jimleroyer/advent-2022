{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module Day9Spec (spec) where

import Control.Monad (forM_)
import Day9
  ( Cell (..),
    Direction (..),
    move,
    parseLine,
    shouldMove,
    solve1,
    solve2,
  )
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck ()
import Text.Printf (printf)
import Text.RawString.QQ (r)
import Prelude hiding (Left, Right)

data TestData a b = TestData {datum :: a, expectation :: b}

dataExample1 =
  [r|R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2|]

dataExample2 =
  [r|R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20|]

spec :: Spec
spec = do
  describe "tail move detection" $ do
    it "does not move when head/tail sit at same cell" $ do
      shouldMove (1, 1) (1, 1) `shouldBe` False

    it "does not move when head/tail sit side by side horizontally and vertically" $ do
      shouldMove (1, 1) (2, 1) `shouldBe` False
      shouldMove (1, 1) (1, 2) `shouldBe` False

    it "does not move when head/tail sit side by side diagonally" $ do
      shouldMove (1, 1) (2, 2) `shouldBe` False

    it "does move when head/tail are too far apart horizontally and vertically" $ do
      shouldMove (1, 1) (3, 1) `shouldBe` True
      shouldMove (1, 1) (1, 3) `shouldBe` True

    it "does move when head/tail are too far apart diagonally" $ do
      shouldMove (1, 1) (3, 3) `shouldBe` True

  describe "tail moves to pursue head" $ do
    it "moves up when head vertically moves up" $ do
      move ((1, 1), (1, 0)) Up `shouldBe` ((1, 2), (1, 1))
      move ((-3, -1), (-3, -2)) Up `shouldBe` ((-3, 0), (-3, -1))

    it "moves up diagonally when head in different column moves up" $ do
      move ((1, 1), (0, 0)) Up `shouldBe` ((1, 2), (1, 1))
      move ((1, 1), (2, 0)) Up `shouldBe` ((1, 2), (1, 1))

    it "stays when head vertically moves up to same cell" $ do
      move ((1, 1), (1, 2)) Up `shouldBe` ((1, 2), (1, 2))

    it "moves down when head vertically moves down" $ do
      move ((1, 1), (1, 2)) Down `shouldBe` ((1, 0), (1, 1))
      move ((-3, -2), (-3, -1)) Down `shouldBe` ((-3, -3), (-3, -2))

    it "moves down diagonally when head in different column moves down" $ do
      move ((1, 1), (0, 2)) Down `shouldBe` ((1, 0), (1, 1))
      move ((1, 1), (2, 2)) Down `shouldBe` ((1, 0), (1, 1))

    it "stays when head vertically moves down to same cell" $ do
      move ((1, 1), (1, 0)) Down `shouldBe` ((1, 0), (1, 0))

    it "moves left when head horizontally moves left" $ do
      move ((1, 1), (2, 1)) Left `shouldBe` ((0, 1), (1, 1))
      move ((-3, -1), (-2, -1)) Left `shouldBe` ((-4, -1), (-3, -1))

    it "moves left diagonally when head in different row moves left" $ do
      move ((1, 1), (2, 0)) Left `shouldBe` ((0, 1), (1, 1))
      move ((1, 1), (2, 2)) Left `shouldBe` ((0, 1), (1, 1))

    it "stays when head horizontally moves left to same cell" $ do
      move ((1, 1), (0, 1)) Left `shouldBe` ((0, 1), (0, 1))

    it "moves right when head horizontally moves right" $ do
      move ((1, 1), (0, 1)) Right `shouldBe` ((2, 1), (1, 1))
      move ((-3, -1), (-4, -1)) Right `shouldBe` ((-2, -1), (-3, -1))

    it "moves right diagonally when head in different row moves right" $ do
      move ((1, 1), (0, 0)) Right `shouldBe` ((2, 1), (1, 1))
      move ((1, 1), (0, 2)) Right `shouldBe` ((2, 1), (1, 1))

    it "stays when head horizontally moves right to same cell" $ do
      move ((1, 1), (2, 1)) Right `shouldBe` ((2, 1), (2, 1))

  describe "solve1" $ do
    it "solves minimal sample" $ do
      let moves = concatMap parseLine (lines dataExample1)
      solve1 moves `shouldBe` 13

  describe "solve2" $ do
    it "solves minimal sample" $ do
      let moves = concatMap parseLine (lines dataExample1)
      solve2 moves `shouldBe` 1

    it "solves larger sample" $ do
      let moves = concatMap parseLine (lines dataExample2)
      solve2 moves `shouldBe` 36
