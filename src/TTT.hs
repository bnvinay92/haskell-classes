module TTT where

import Data.Maybe (isNothing)

data Player
  = X
  | O
  deriving (Eq, Show)

data TTCell =
  TTCell Int
         Int
  deriving (Eq, Show)

type TTMove = (TTCell, Player)

type Size = Int

data TTBoard = TTBoard
  { history :: [TTMove]
  , len :: Size
  }

type Strategy = TTBoard -> TTBoard

print' (TTBoard moves size) = gridPrint (grid size)
  where
    gridPrint (row:rows) = rowPrint row ++ "\n" ++ gridPrint rows
    gridPrint [] = []
    rowPrint (cell:cells) = playerPrint (lookup cell moves) ++ " " ++ rowPrint cells
    rowPrint [] = []
    playerPrint (Just player) = show player
    playerPrint Nothing = "_"

grid :: Int -> [[TTCell]]
grid size = [[TTCell x y | y <- [0 .. size - 1]] | x <- [0 .. size - 1]]

victoryLines :: Size -> [[TTCell]]
victoryLines size = rows ++ cols ++ [leftDiag] ++ [rightDiag]
  where
    rows = grid size
    cols = [[TTCell x y | x <- [0 .. size - 1]] | y <- [0 .. size - 1]]
    leftDiag = [TTCell x x | x <- [0 .. size - 1]]
    rightDiag = [TTCell x ((size - 1) - x) | x <- [0 .. size - 1]]

isGameOver board@(TTBoard movesMap size) =
  let hasWon p = any (all (== Just p) . playedBy) (victoryLines size)
      playedBy = map (`lookup` movesMap)
  in null (legalMoves board) || hasWon X || hasWon O

legalMoves (TTBoard movesMap size) =
  let nPlayer = nextPlayer movesMap
      unplayedCells = filter isNotPlayed (concat (grid size))
      isNotPlayed cell = isNothing (lookup cell movesMap)
      nextPlayer [] = X
      nextPlayer ((c, p):ms) =
        case p of
          X -> O
          O -> X
  in map (\unplayedCell -> (unplayedCell, nPlayer)) unplayedCells

play move (TTBoard moves size) = TTBoard (move : moves) size

newBoard = TTBoard [] 3

game' :: TTBoard -> Strategy -> String
game' board strategy =
  if isGameOver board
    then print' board
    else game' (strategy board) strategy

game b s = putStrLn (game' b s)

playFirstMove :: Strategy
playFirstMove b = play (head (legalMoves b)) b