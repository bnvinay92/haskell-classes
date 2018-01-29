module Sudoku where

import           Control.Arrow (second)
import           Data.Array    (Array, array, assocs, elems, (!), (//))
import           Data.Char     (digitToInt)
import           Data.List     (delete, intercalate, nub, sort)
import           Debug.Trace

type Digit = Int
type Coord = (Int, Int)
type DistinctSpace = [Coord]
type Board = Array Coord [Digit]

s1 = "......52..8.4......3...9...5.1...6..2..7........3.....6...1..........7.4.......3."
s2 = "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"
s3 = "8..7....4.5....6............3.97...8....43..5....2.9....6......2...6...7.71..83.2"

spaceSize = 9
root = head [x | x <- [1 .. spaceSize], x * x == spaceSize]
allDigits = [1 .. spaceSize]
indexes = [0 .. spaceSize - 1]

foldMayBees :: (Board -> a -> Maybe Board) -> Board -> [a] -> Maybe Board
foldMayBees f b [] = Just b
foldMayBees f b [a] = f b a
foldMayBees f b (x:xs) =
  case f b x of
    (Just board) -> foldMayBees f board xs
    _            -> Nothing

andThen :: Maybe Board -> (Board -> Maybe Board) -> Maybe Board
andThen (Just b) f = f b
andThen _ _        = Nothing

readBoard :: String -> Maybe Board
readBoard str = foldMayBees assign emptyBoard assignments
  where
    emptyBoard = array ((0, 0), (spaceSize - 1, spaceSize - 1)) $ zip allCoords $ repeat allDigits
    assignments = map (second digitToInt) . filter isDigit $ zip allCoords str
    isDigit = (`elem` concatMap show allDigits) . snd
    allCoords = [(x, y) | x <- indexes, y <- indexes]

ppBoard :: Board -> String
ppBoard = unlines . map unwords . splitHorizontally . splitVertically . groupDigits . toDigits
  where
    toDigits = map ppDigit . elems
    ppDigit [d] = show d
    ppDigit _   = "."
    groupDigits = splitEvery root . splitEvery root . splitEvery root
    splitVertically = map (map (intercalate ["|"]))
    splitHorizontally = intercalate [[line]]
    line = "- - - + - - - + - - -"
    splitEvery n [] = []
    splitEvery n xs = head : splitEvery n tail
      where
        (head, tail) = splitAt n xs

assign :: Board -> (Coord, Digit) -> Maybe Board
assign board (c, d) = foldMayBees eliminate board eliminations
  where
    eliminations = zip (repeat c) $ delete d $ board ! c

eliminate :: Board -> (Coord, Digit) -> Maybe Board
eliminate b (c, d) =
  if d `elem` guesses
    then strat1 assignment newBoard `andThen` strat2 (c, d)
    else Just b
  where
    newBoard = b // [assignment]
    assignment = (c, delete d guesses)
    guesses = b ! c

-- If 'd' is a fixed digit, eliminate it from peers.
strat1 :: (Coord, [Digit]) -> Board -> Maybe Board
strat1 (c, ds) b =
  case ds of
    [] -> Nothing
    [d] -> foldMayBees eliminate b (zip peersOfC (repeat d))
      where peersOfC = (nub . concat . spacesContaining) c
    _ -> Just b

-- If 'd' appears in exactly one coord of a space, then fix it there.
strat2 :: (Coord, Digit) -> Board -> Maybe Board
strat2 (c, d) b = foldMayBees (tryAssigning d) b (spacesContaining c)
  where
    tryAssigning d board space =
      case filter ((d `elem`) . (board !)) space of
        []  -> Nothing
        [s] -> assign board (s, d)
        _   -> Just board

spacesContaining :: Coord -> [DistinctSpace]
spacesContaining (r, c) = [row, col, grid]
  where
    row = [(r, y) | y <- indexes, y /= c]
    col = [(x, c) | x <- indexes, x /= r]
    grid = [(x, y) | x <- [gx .. gx + root - 1], y <- [gy .. gy + root - 1], (x, y) /= (r, c)]
    (gx, gy) = (root * quot r root, root * quot c root)
    root = head [x | x <- [1 .. spaceSize], x * x == spaceSize]

solve :: Board -> Maybe Board
solve b =
  case possibleGuesses of
    [] -> Just b
    _ ->
      let (_, (c, ds)) = minimum possibleGuesses
      -- 'reverse ds' is key to beating abnv's benchmark.
      in firstJust [assign b (c, d) `andThen` solve | d <- reverse ds]
  where
    possibleGuesses = [(size, (c, ds)) | (c, ds) <- assocs b, let size = length ds, size /= 1]
    firstJust []              = Nothing
    firstJust (x@(Just j):xs) = x
    firstJust (_:xs)          = firstJust xs

sudoku :: String -> IO ()
sudoku s =
  let Just b = readBoard s `andThen` solve
  in putStr . ppBoard $ b
