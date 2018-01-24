module Sudoku where

import           Data.Char  (digitToInt)
import           Data.List  (intercalate)
import           Data.Map   (Map, fromList, insert, lookup, toAscList)
import           Data.Maybe (fromJust)

data Cell =
  Cell Int
       Int
  deriving (Eq, Show)

instance Ord Cell where
  compare (Cell a b) (Cell x y) = compare (a * 9 + b) (x * 9 + y)

data Digit
  = Fixed Int
  | OneOf [Int]
  deriving (Eq, Show)

type Space = [Cell]

type Board = Map Cell Digit

testBoard = readBoard "003020600900305001001806400008102900700000008006708200002609500800203009005010300"

row :: Int -> Space
row x = [Cell x y | y <- [0 .. 8]]

find :: Board -> Cell -> Digit
find board cell = fromJust $ Data.Map.lookup cell board

readBoard :: String -> Board
readBoard str = fromList $ zip (concatMap row [0 .. 8]) [toDigit s | s <- str]
  where
    toDigit '0' = OneOf [1 .. 9]
    toDigit c   = Fixed $ digitToInt c

ppBoard board = unlines $ intercalate [line] sections
  where
    sections = map (map (unwords . intercalate ["|"])) digitsChunked
    digitsChunked = (splitEvery 3 . splitEvery 3 . splitEvery 3) digitList
    digitList = map (ppDigit . snd) $ toAscList board
    ppDigit (Fixed d) = show d
    ppDigit _         = "."
    line = "- - - + - - - + - - -"
    splitEvery n [] = []
    splitEvery n lst =
      let (head, tail) = splitAt n lst
      in head : splitEvery n tail
