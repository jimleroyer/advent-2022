{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Day5
  ( intoStep,
    Step (..),
    move9000,
    move9001,
    solve1,
    solve2,
    main,
  )
where

import Control.Lens
import Data.List (intersect, isInfixOf, isSubsequenceOf)
import System.IO ()
import Text.Printf (printf)
import Text.RawString.QQ (r)
import Text.Regex.Posix

filename :: FilePath
filename = "./data/day05-input.txt"

data Step = Step {from :: Int, to :: Int, num :: Int} deriving (Eq, Show)

type Stack = [Char]

dataStacks :: [Stack]
dataStacks = ["SPHVFG", "MZDVBFJG", "NJLMG", "PWDVZGN", "BCRV", "ZLWPMSRV", "PHT", "VZHCNSRQ", "JQVPGLF"]

intoStep :: String -> Step
intoStep strStep = Step from to num
  where
    pattern = "move ([0-9]+) from ([0-9]+) to ([0-9]+)" :: String
    matches = tail (head (strStep =~ pattern :: [[String]]))
    (num, from, to) = (read (matches !! 0) :: Int, read (matches !! 1) :: Int, read (matches !! 2) :: Int)

move :: ([Char] -> [Char]) -> [Stack] -> Step -> [Stack]
move ordering stacks step = finalStacks
  where
    from = stacks !! (step.from - 1)
    to = stacks !! (step.to - 1)
    (crates, newFrom) = splitAt (step.num) from
    newTo = ordering crates ++ to
    movingStacks = (element (step.from - 1) .~ newFrom) stacks
    finalStacks = (element (step.to - 1) .~ newTo) movingStacks

move9000 :: [Stack] -> Step -> [Stack]
move9000 = move reverse

move9001 :: [Stack] -> Step -> [Stack]
move9001 = move id

solve1 :: [Step] -> [Stack] -> [Char]
solve1 steps stacks = map head (foldl move9000 stacks steps)

solve2 :: [Step] -> [Stack] -> [Char]
solve2 steps stacks = map head (foldl move9001 stacks steps)

main :: IO ()
main = do
  lines <- drop 10 . lines <$> readFile filename
  let steps = map intoStep lines
  let topCrates9000 = solve1 steps dataStacks
  let topCrates9001 = solve2 steps dataStacks
  printf "Part 1: %s, Part 2: %s" topCrates9000 topCrates9001
