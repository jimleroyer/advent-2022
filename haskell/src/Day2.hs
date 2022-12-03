{-# LANGUAGE OverloadedStrings #-}

module Day2 (part1, part2) where

import Control.Applicative ()
import qualified Data.ByteString.Lazy.Char8 as B (lines, readFile, unpack)
import Data.List ()
import Data.Typeable (typeOf)
import System.IO ()

filename :: FilePath
filename = "./data/day02-input.txt"

data Move = Rock | Paper | Scissor deriving (Show, Eq, Enum)

data Game = Win | Draw | Lose deriving (Show, Eq)

intoMove :: Char -> Move
intoMove 'A' = Rock
intoMove 'B' = Paper
intoMove 'C' = Scissor
intoMove 'X' = Rock
intoMove 'Y' = Paper
intoMove 'Z' = Scissor

intoMoves :: [Char] -> (Move, Move)
intoMoves (x : ' ' : y : _) = (intoMove x, intoMove y)

intoPlan :: Char -> Game
intoPlan 'X' = Lose
intoPlan 'Y' = Draw
intoPlan 'Z' = Win

winOver :: Move -> Move
winOver m
  | m == Rock = Scissor
  | otherwise = pred m

loseOver :: Move -> Move
loseOver m
  | m == Scissor = Rock
  | otherwise = succ m

scorePlan :: [Char] -> Int
scorePlan (x : ' ' : y : _)
  | p == Win = score hisMove (loseOver hisMove)
  | p == Draw = score hisMove hisMove
  | p == Lose = score hisMove (winOver hisMove)
  where
    (hisMove, p) = (intoMove x, intoPlan y)

play :: Move -> Move -> Game
play hisMove myMove
  | hisMove == myMove = Draw
  | hisMove == winOver myMove = Win
  | hisMove == loseOver myMove = Lose

scoreMove :: Move -> Int
scoreMove Rock = 1
scoreMove Paper = 2
scoreMove Scissor = 3

scoreGame :: Game -> Int
scoreGame Win = 6
scoreGame Draw = 3
scoreGame Lose = 0

score :: Move -> Move -> Int
score hisMove myMove = scoreGame (play hisMove myMove) + scoreMove myMove

part1 :: IO ()
part1 = do
  lines <- B.lines <$> B.readFile filename
  let total = sum $ map (uncurry score . intoMoves . B.unpack) lines
  print total

part2 :: IO ()
part2 = do
  lines <- B.lines <$> B.readFile filename
  let total = sum $ map (scorePlan . B.unpack) lines
  print total
