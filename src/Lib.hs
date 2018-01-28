module Lib where

--The toDigits function converts an integer into a list of its single digits.
--
--toDigits 1234 == [1,2,3,4]
--toDigits 0 == []
--The doubleEveryOther function doubles the values of every second digit beginning from the right.
--
--doubleEveryOther [8,7,6,5] == [16,7,12,5]
--doubleEveryOther [1,2,3] == [1,4,3]
toDigits 0 = []
toDigits x = toDigits (quot x 10) ++ [rem x 10]

doubleEveryOther [] = []
doubleEveryOther xs' =
  let xs = reverse xs'
  in reverse $ doubleEO xs
  where
    doubleEO (x1:x2:xs) = x1 : (2 * x2) : doubleEO xs
    doubleEO xs         = xs

--
chopHelper :: Int -> [Int] -> [Int]
chopHelper _ []         = []
chopHelper 1 [x]        = [max (x - 1) 0]
chopHelper level (0:xs) = 0 : chopHelper (level - 1) xs
chopHelper level (x:xs) = x - 1 : chopEnd (level - 1) xs

chopEnd c (x:xs) = (x + c) : xs

chop :: [Int] -> [Int]
chop x = chopHelper (length x) x

-- POTD 1
data Op
  = Plus
  | Minus
  | Times

data Tree
  = TInt Integer
  | TOp String
        Tree
        Tree
  deriving (Show, Eq)

swap :: Tree -> Tree
swap (TOp op left right) = TOp op (swap right) (swap left)
swap tint                = tint

calc :: Tree -> Integer
calc (TOp op left right) =
  case op of
    "*" -> calc left * calc right
    "+" -> calc left + calc right
    "-" -> calc left - calc right
calc (TInt int) = int

t1 = TOp "*" (TOp "+" (TInt 20) (TInt 1)) (TOp "-" (TInt 10) (TInt 8))

-- POTD 2
signum n
  | n < 0 = -1
  | n > 0 = 1
  | otherwise = 0

if' cond then' else' =
  if cond
    then then'
    else else'
