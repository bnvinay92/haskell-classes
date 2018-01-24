module Lib where

--  [0,0,2,0,0,0,0,0,0] -> [0,0,1,6,0,0,0,0,1]
chopHelper :: Int -> [Int] -> [Int]
chopHelper _ []         = []
chopHelper 1 [x]        = [max (x - 1) 0]
chopHelper level (0:xs) = 0 : chopHelper (level - 1) xs
chopHelper level (x:xs) = x - 1 : chopEnd (level - 1) xs

chopEnd c (x:xs) = (x + c) : xs

chop :: [Int] -> [Int]
chop x = chopHelper (length x) x
--
