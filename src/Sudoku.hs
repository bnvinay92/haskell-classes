module Sudoku where

import           Data.Char  (digitToInt)
import           Data.List  (intercalate, partition, sort, sortOn, transpose, (\\))
import           Data.Map   (Map, elems, fromList, insert, lookup, toAscList, union)
import           Data.Maybe (fromJust)
import           Prelude    hiding (lookup)

type Board = Map Coord Digit
type Coord = (Int, Int)
type Digit = [Int]
type Space = [Coord]

cardBoard = readBoard "020810740700003100090002805009040087400208003160030200302700060005600008076051090"
sandBoard = readBoard "030050040008010500460000012070502080000603000040109030250000098001020600080060020"
hardBoard = readBoard "100920000524010000000000070050008102000000000402700090060000000000030945000071006"
deccanBoard = readBoard "..36....92..1..6.86...8.2........4.7..64791..8.4........2.4...17.1..6..24....57.."
deccanXBoard = readBoard ".826..........3...43......5......7..2..7.....3...8...4...1.4.2....2....3..85....7"

size = 9 :: Int
root = head [x | x <- [1 .. size], x * x == size] :: Int
rowSpaces = [[(x, y) | y <- [0 .. size - 1]] | x <- [0 .. size - 1]]

allSpaces :: [[Coord]]
allSpaces = rowSpaces ++ colSpaces ++ gridSpaces
  where
    colSpaces = transpose rowSpaces
    gridSpaces = map grid origins
      where
        origins = [(x, y) | x <- [0,root .. size - 1], y <- [0,root .. size - 1]]
        grid (x, y) = [(a, b) | a <- [x .. x + root - 1], b <- [y .. y + root - 1]]

readBoard :: String -> Board
readBoard str = fromList $ zip coords digits
  where
    coords = concat rowSpaces
    digits = map toDigit str
    toDigit c
      | c == '0' || c == '.' = [1 .. 9]
      | otherwise = [digitToInt c]

ppBoard :: Board -> IO ()
ppBoard = putStr . unlines . map unwords . splitHorizontally . splitVertically . groupDigits . toDigits
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

digits :: Board -> Space -> [Digit]
digits b = map (fromJust . (`lookup` b))

isFixed :: Digit -> Bool
isFixed = (==) 1 . length

prune :: Board -> Board
prune b =
  if pruned == b
    then pruned
    else prune pruned
  where
    pruned = foldl pruneSpace b allSpaces
    pruneSpace board space =
      let ds = digits board space
          prunedDigits = map (remove $ fixedList ds) ds
      in union (fromList $ zip space prunedDigits) board
    remove _ [x]                = [x]
    remove alreadyFixed guesses = guesses \\ alreadyFixed
    fixedList = concat . filter ((==) 1 . length)

solve :: Board -> IO ()
solve b =
  case solve' $ prune b of
    (Just board) -> ppBoard board
    _            -> putStrLn "No Solution!"

solve' :: Board -> Maybe Board
solve' b =
  case (isFilled, isValid) of
    (True, True) -> Just b
    (False, True) -> firstJust (\guess@(c, d) -> solve' $ prune $ insert c d b) $ allGuesses b
    _ -> Nothing
  where
    allGuesses = concatMap (\(k, cs) -> [(k, [c]) | c <- cs]) . sortOn length . filter (not . isFixed . snd) . toAscList
    isFilled = all ((==) 1 . length) (elems b)
    isValid = all (hasDistinct . concat . filter isFixed . digits b) allSpaces
      where
        hasDistinct [] = True
        hasDistinct xs =
          let sorted = sort xs
          in and $ zipWith (/=) sorted (drop 1 sorted)
    firstJust _ [] = Nothing
    firstJust f (g:gs) =
      let m = f g
      in case m of
           x@(Just _) -> x
           _          -> firstJust f gs
