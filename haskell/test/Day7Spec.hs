{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module Day7Spec (spec) where

import Control.Monad (forM_)
import Data.Either (fromRight, isLeft, isRight, rights)
import Data.Map (fromList)
import Data.Map as M hiding (size)
import Day7
  ( Line (..),
    navigate,
    parseLine,
    solve1,
    solve2,
  )
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck ()
import Text.Printf (printf)
import Text.RawString.QQ (r)

data TestData a b = TestData {datum :: a, expectation :: b}

spec :: Spec
spec = do
  describe "input lines parsing" $ do
    it "parses cd command with named directory" $ do
      let parsed = parseLine "$ cd foobar"
      isRight parsed `shouldBe` True
      rights [parsed] `shouldBe` [CdCommand {directory = "foobar"}]

    it "parses cd command with the root directory" $ do
      let parsed = parseLine "$ cd /"
      isRight parsed `shouldBe` True
      rights [parsed] `shouldBe` [CdCommand {directory = "/"}]

    it "parses cd command with the parent directory" $ do
      let parsed = parseLine "$ cd .."
      isRight parsed `shouldBe` True
      rights [parsed] `shouldBe` [CdCommand {directory = ".."}]

    it "parses ls command" $ do
      let parsed = parseLine "$ ls"
      isRight parsed `shouldBe` True
      rights [parsed] `shouldBe` [LsCommand]

    it "parses file line" $ do
      let parsed = parseLine "163455 jlwjsbw.bzf"
      isRight parsed `shouldBe` True
      rights [parsed] `shouldBe` [FileEntry {filename = "jlwjsbw.bzf", size = 163455}]

    it "parses directory line" $ do
      let parsed = parseLine "dir jqlm"
      isRight parsed `shouldBe` True
      rights [parsed] `shouldBe` [DirectoryEntry {dirname = "jqlm"}]

    it "parses junk input and reports error" $ do
      let parsed = parseLine "junk input"
      isLeft parsed `shouldBe` True

  describe "navigation" $ do
    it "initiates properly on cd root" $ do
      let navigated = navigate M.empty [] [CdCommand {directory = "/"}]
      navigated `shouldBe` M.empty

    it "can change directory" $ do
      let navigated = navigate (fromList [(["/"], 10)]) ["/"] [CdCommand {directory = "/users"}]
      navigated `shouldBe` fromList [(["/"], 10)]

    it "list and compute directories size, including parents" $ do
      let state = fromList [(["/"], 300), (["/", "users"], 10)]
      let dirs = ["/", "users"]
      let navigated = navigate state dirs [FileEntry {filename = "jlwjsbw.bzf", size = 163455}]
      navigated `shouldBe` fromList [(["/"], 163755), (["/", "users"], 163465)]

  describe "solve1" $ do
    it "computes total size of directories under size of 100000" $ do
      let lines = [CdCommand {directory = "/"}, LsCommand, DirectoryEntry {dirname = "users"}, FileEntry {filename = "init.bzf", size = 290}, CdCommand {directory = "users"}, LsCommand, FileEntry {filename = "jlwjsbw.bzf", size = 163455}, CdCommand {directory = "jlr"}, LsCommand, FileEntry {filename = "config.ini", size = 1024}, CdCommand {directory = ".."}, CdCommand {directory = "babaghanoush"}, FileEntry {filename = "food.txt", size = 10000}]
      solve1 lines `shouldBe` 11024
