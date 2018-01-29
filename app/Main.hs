module Main where

import           Sudoku
import           System.Environment

lolfast s =
  case readBoard s of
    Nothing -> putStrLn "Invalid board"
    Just b ->
      case solve b of
        Nothing -> putStrLn "No Solution"
        _ -> return ()

main :: IO ()
main = do
  s <- getLine
  grids <- lines `fmap` readFile s
  mapM_ lolfast grids

