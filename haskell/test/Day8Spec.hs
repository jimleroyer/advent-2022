{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module Day8Spec (spec) where

import Control.Monad (forM_)
import Data.Either (fromRight, isLeft, isRight, rights)
import Data.Map (fromList)
import Data.Map as M hiding (size)
import Day8
  ( Grid (..),
    Tree (..),
    east,
    height,
    intoGrid,
    north,
    scenicScore,
    scenicViews,
    solve1,
    solve2,
    south,
    visibleTrees,
    west,
  )
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck ()
import Text.Printf (printf)
import Text.RawString.QQ (r)

dataExample :: String
dataExample =
  [r|30373
25512
65332
33549
35390|]

dataGrid :: Grid
dataGrid = intoGrid (lines dataExample)

data TestData a b = TestData {datum :: a, expectation :: b}

spec :: Spec
spec = do
  describe "directions" $ do
    it "provides trees at north" $ do
      north dataGrid (2, 3) `shouldBe` [(2, 2), (2, 1), (2, 0)]

    it "provides trees at south" $ do
      south dataGrid (2, 1) `shouldBe` [(2, 2), (2, 3), (2, 4)]

    it "provides trees at east" $ do
      east dataGrid (0, 4) `shouldBe` [(1, 4), (2, 4), (3, 4), (4, 4)]

    it "provides trees at west" $ do
      west dataGrid (4, 0) `shouldBe` [(3, 0), (2, 0), (1, 0), (0, 0)]

    it "handled trees at edge" $ do
      west dataGrid (0, 0) `shouldBe` ([] :: [Tree])

  describe "visible trees" $ do
    it "produces list of trees that are visible with minimal sample" $ do
      let grid = [[2, 2, 2], [2, 3, 2], [2, 2, 2]]
      visibleTrees grid `shouldBe` [(1, 1)]

    it "produces list of trees that are visible with example sample" $ do
      visibleTrees dataGrid `shouldBe` [(1, 1), (2, 1), (1, 2), (3, 2), (2, 3)]

  describe "solve1" $ do
    it "computes total size of directories under size of 100000" $ do
      solve1 dataGrid `shouldBe` 21

  describe "scenic score" $ do
    it "computes the view for one tree with minimal sample" $ do
      let grid = [[2, 2, 2], [2, 3, 2], [2, 2, 2]]
      scenicScore grid (1, 1) `shouldBe` 1
      scenicScore grid (0, 0) `shouldBe` 0
      scenicScore grid (2, 2) `shouldBe` 0

    it "computes the view for one tree with example sample" $ do
      scenicScore dataGrid (2, 1) `shouldBe` 4
      scenicScore dataGrid (2, 3) `shouldBe` 8