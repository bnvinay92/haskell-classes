module TicTacToe where

data Player
  = X
  | O
  deriving (Eq, Show)

data TTCell =
  TTCell Int
         Int
  deriving (Eq, Show)

data TTMove = TTMove
  { player :: Player
  , cell :: TTCell
  } deriving (Eq, Show)

data TTBoard = TTBoard
  { moveHistory :: [TTMove]
  , emptyCells :: [TTCell]
  } deriving (Eq, Show)

isGameOver :: TTBoard -> Bool
isGameOver (TTBoard (lastMove:moves) remainingCells) =
  null remainingCells ||
  (let lastPlayerMoves = filter (\move -> player move == player lastMove) moves
       lastPlayerCells = map cell lastPlayerMoves
   in 3 `elem` recWinSum [topRow, midRow, botRow, fstCol, midCol, lstCol, leftDiag, rightDiag] lastPlayerCells)

recWinSum :: [TTCell -> Bool] -> [TTCell] -> [Int]
recWinSum winFilters =
  foldl
    (\runningSums c ->
       let cellMemberships = map boolToInt (sequence winFilters c)
       in zipWith (+) runningSums cellMemberships)
    (replicate (length winFilters) 0)

calcLegalMoves :: TTBoard -> [TTMove]
calcLegalMoves (TTBoard moves remainingCells) = map (TTMove (nextPlayer moves)) remainingCells

lastPlayer :: [TTMove] -> Player
lastPlayer (move:moves) = player move

nextPlayer :: [TTMove] -> Player
nextPlayer [] = X
nextPlayer moves = itsNot (lastPlayer moves)

boolToInt True = 1
boolToInt False = 0

itsNot X = O
itsNot O = X

topRow (TTCell r _) = r == 0

midRow (TTCell r _) = r == 1

botRow (TTCell r _) = r == 2

fstCol (TTCell _ c) = c == 0

midCol (TTCell _ c) = c == 1

lstCol (TTCell _ c) = c == 2

leftDiag (TTCell r c) = r == c

rightDiag (TTCell r c) = r + c == 2