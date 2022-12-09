{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Day7
  ( solve1,
    solve2,
    main,
    Line (..),
    navigate,
    parseLine,
  )
where

import Control.Applicative hiding (many, some)
import Control.Monad.Combinators (choice, many, manyTill, some)
import Control.Monad.Combinators as P
import Data.Char
import Data.List (find, inits, sort, tails)
import Data.Map (Map, elems, keys, lookup, member, null)
import Data.Map as M hiding (filter, foldr, insertWith, size)
import Data.Map.Strict (insertWith)
import Data.Maybe (fromMaybe)
import Data.Void
import System.IO ()
import Text.Megaparsec (ParseError, parse, try)
import Text.Megaparsec as P
import Text.Megaparsec.Char (alphaNumChar, char, eol, space, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Printf (printf)

inputFilename :: FilePath
inputFilename = "./data/day07-input.txt"

-- Models ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data Line
  = CdCommand {directory :: String}
  | LsCommand
  | FileEntry {filename :: String, size :: Int}
  | DirectoryEntry {dirname :: String}
  deriving (Eq, Show)

-- Parser ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

type Parser = Parsec Void String

type ParserError = ParseErrorBundle String Void

regularParse :: Parser a -> String -> Either ParserError a
regularParse p = parse p ""

lineParser :: Parser Line
lineParser = cdParser <|> lsParser <|> dirParser <|> fileParser
  where
    cdParser = do
      _ <- string "$ cd "
      directory <- string ".." <|> string "/" <|> some alphaNumChar
      return CdCommand {directory = directory}
    lsParser = do
      _ <- string "$ ls"
      return LsCommand
    dirParser = do
      _ <- string "dir"
      _ <- space
      dirname <- some alphaNumChar
      return DirectoryEntry {dirname = dirname}
    fileParser = do
      size <- decimal
      _ <- space
      filename <- some (char '.' <|> alphaNumChar)
      return FileEntry {filename = filename, size = size}

linesParser :: Parser [Line]
linesParser = sepEndBy1 lineParser eol

parseLine :: String -> Either ParserError Line
parseLine = regularParse lineParser

parseLines :: String -> Either ParserError [Line]
parseLines = regularParse linesParser

-- Directory processing ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- Store directories in a map, where key is a path and value is a size. The key
-- as a hierarchy of path is made of an array of string for each folders. This
-- allows the program to easily propagate the size back to the parent folders.
type Directories = Map [String] Int

navigate :: Directories -> [String] -> [Line] -> Directories
navigate mapState dirHierarchy [] = mapState
navigate mapState dirHierarchy (currentLine : lines) =
  case currentLine of
    CdCommand "/" -> navigate mapState ["/"] lines
    CdCommand ".." -> navigate mapState (init dirHierarchy) lines
    CdCommand dest -> navigate mapState (dirHierarchy ++ [dest]) lines
    FileEntry {filename, size} -> navigate newDirs dirHierarchy lines
      where
        dirsHierarchy = tail (inits dirHierarchy)
        newDirs = foldr (\d m -> insertWith (+) d size m) mapState dirsHierarchy
    _ -> navigate mapState dirHierarchy lines

-- Main logic ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

solve1 :: [Line] -> Int
solve1 lines = sum (filter (< 100000) (elems dirs))
  where
    dirs = navigate M.empty [] lines

solve2 :: [Line] -> Maybe Int
solve2 lines = smallest
  where
    dirs = navigate M.empty [] lines
    sortedDirs = sort (M.elems dirs)
    usedSpace = last sortedDirs
    unusedSpace = 70000000 - usedSpace
    smallest = find (\dir -> unusedSpace + dir >= 30000000) sortedDirs

main :: IO ()
main = do
  parsed <- parseLines <$> readFile inputFilename
  case parsed of
    Right instructions -> do
      let total1 = solve1 instructions
      let total2 = fromMaybe 0 (solve2 instructions)
      printf "Part 1: %d, Part 2: %d" total1 total2
    Left errors -> print "invalid input file"
