module Sudoku where

import           Data.Char  (digitToInt)
import           Data.List  (intercalate, sort, sortOn)
import           Data.Map   (Map, fromList, insert, lookup, toAscList)
import           Data.Maybe (fromJust)

data Cell =
  Cell Int
       Int
  deriving (Eq, Show)

instance Ord Cell where
  compare (Cell a b) (Cell x y) = compare (a * 9 + b) (x * 9 + y)

type Digit = [Int]

type Board = Map Cell Digit

testBoard = readBoard ".....6....59.....82....8....45........3........6..3.54...325..6.................."

easyBoard = readBoard "003020600900305001001806400008102900700000008006708200002609500800203009005010300"

rows = [[Cell x y | y <- [0 .. 8]] | x <- [0 .. 8]]

cols = [[Cell x y | x <- [0 .. 8]] | y <- [0 .. 8]]

grids = map grid origins
  where
    origins = [Cell x y | x <- [0, 3, 6], y <- [0, 3, 6]]
    grid (Cell x y) = [Cell a b | a <- [x .. x + 2], b <- [y .. y + 2]]

readBoard str = fromList $ zip (concat rows) [toDigit s | s <- str]
  where
    toDigit c
      | c == '0' || c == '.' = [1 .. 9]
      | otherwise = [digitToInt c]

ppBoard board = unlines $ intercalate [line] sections
  where
    ppDigit [d] = show d
    ppDigit _   = "."
    sections = map (map (unwords . intercalate ["|"])) digitsChunked
      where
        digitsChunked = (splitEvery 3 . splitEvery 3 . splitEvery 3) digitList
        digitList = map (ppDigit . snd) $ toAscList board
    line = "- - - + - - - + - - -"
    splitEvery n [] = []
    splitEvery n lst =
      let (head, tail) = splitAt n lst
      in head : splitEvery n tail

solve board =
  case solve' board of
    (Just b) -> putStrLn $ ppBoard b
    _        -> putStrLn "No solution"

solve' board =
  let complete = isFilled board
      valid = isValid board
  in case (complete, valid) of
       (True, True) -> Just board
       (_, True) -> firstJust guessList (\(c, d) -> solve' $ insert c [d] board)
       _ -> Nothing
  where
    guessList =
      concatMap (\(cell, digits) -> [(cell, x) | x <- digits]) $
      sortOn (length . snd) $ filter (\d -> length (snd d) > 1) $ toAscList board
    firstJust [] f = Nothing
    firstJust (x:xs) f =
      let r = f x
      in case r of
           j@(Just _) -> j
           _          -> firstJust xs f

isValid board = all (isValidSpace . map (find board)) (rows ++ cols ++ grids)
  where
    find board cell = fromJust $ Data.Map.lookup cell board
    isValidSpace :: [Digit] -> Bool
    isValidSpace = comparePairwise . sort . filter (\d -> length d == 1)
    comparePairwise xs = and $ zipWith (/=) xs (drop 1 xs)

isFilled board = all (\lst -> length lst == 1) $ map snd $ toAscList board
