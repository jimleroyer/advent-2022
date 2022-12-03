{-# LANGUAGE OverloadedStrings #-}

module Day2 (part1, part2) where

import Control.Applicative ()
import qualified Data.ByteString.Lazy.Char8 as B (lines, readFile, unpack)
import Data.List ()
import Data.Typeable (typeOf)
import System.IO ()

filename :: FilePath
filename = "./data/day02-input.txt"

data Move = Rock | Paper | Scissor deriving (Show, Eq)

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

scorePlan :: [Char] -> Int
scorePlan (x : ' ' : y : _)
  | m == Rock && p == Win = score Rock Paper
  | m == Rock && p == Lose = score Rock Scissor
  | m == Rock && p == Draw = score Rock Rock
  | m == Paper && p == Win = score Paper Scissor
  | m == Paper && p == Lose = score Paper Rock
  | m == Paper && p == Draw = score Paper Paper
  | m == Scissor && p == Win = score Scissor Rock
  | m == Scissor && p == Lose = score Scissor Paper
  | m == Scissor && p == Draw = score Scissor Scissor
  where
    (m, p) = (intoMove x, intoPlan y)

-- play:: hisMove -> myMove -> Result
play :: Move -> Move -> Game
play Rock Paper = Win -- his rock against my paper: I win
play Rock Scissor = Lose
play Rock Rock = Draw
play Paper Rock = Lose -- his paper against my rock: I lose
play Paper Scissor = Win
play Paper Paper = Draw
play Scissor Rock = Win
play Scissor Paper = Lose
play Scissor Scissor = Draw

scoreMove :: Move -> Int
scoreMove Rock = 1
scoreMove Paper = 2
scoreMove Scissor = 3

scoreGame :: Game -> Int
scoreGame Win = 6
scoreGame Draw = 3
scoreGame Lose = 0

-- score:: hisMove -> myMove -> result
score :: Move -> Move -> Int
score hisMove myMove = scoreGame (play hisMove myMove) + scoreMove myMove

main :: IO ()
main = do
  lines <- B.lines <$> B.readFile filename
  print lines

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
