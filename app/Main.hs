module Main where

main :: IO ()
main = interact reverse

intList = List 1 (List 2 (List 3 EmptyList))

stringList = List 'a' (List 'b' (List 'c' EmptyList))

data List a
  = List a
         (List a)
  | EmptyList
  deriving (Show)

lst :: List a -> Maybe a
lst (List head EmptyList) = Just head
lst (List head tail) = lst tail
lst EmptyList = Nothing

data Btree a
  = Btree { value :: a
          , left :: Btree a
          , right :: Btree a }
  | EmptyTree
  deriving (Show)

intTree =
  Btree
    5
    (Btree 3 (Btree 1 EmptyTree EmptyTree) (Btree 4 EmptyTree EmptyTree))
    (Btree 7 (Btree 6 EmptyTree EmptyTree) (Btree 8 EmptyTree EmptyTree))

isBst :: (Ord a) => Btree a -> Bool
isBst EmptyTree = True
isBst (Btree val EmptyTree EmptyTree) = True
isBst (Btree val left EmptyTree) = value left <= val && isBst left
isBst (Btree val EmptyTree right) = value right >= val && isBst right
isBst (Btree val left right) = value left <= val && value right >= val && isBst left && isBst right

srch :: (Ord a) => a -> Btree a -> Bool
srch x EmptyTree = False
srch x (Btree value left right)
  | x == value = True
  | x < value = srch x left
  | otherwise = srch x right

insrt :: (Ord a) => a -> Btree a -> Btree a
insrt a EmptyTree = Btree a EmptyTree EmptyTree
insrt a (Btree value left right) =
  if a < value
    then Btree value (insrt a left) right
    else Btree value left (insrt a right)

backspace :: (Ord a) => a -> Btree a -> Btree a
backspace a EmptyTree = EmptyTree
backspace a (Btree val left right)
  | a == val =
    let replacement = rightmost left
    in case replacement of
         Just x -> Btree x (backspace x left) right
         Nothing -> right
  | a < val = Btree val (backspace a left) right
  | otherwise = Btree val left (backspace a right)

rightmost :: Btree a -> Maybe a
rightmost EmptyTree = Nothing
rightmost (Btree val left EmptyTree) = Just val
rightmost (Btree val left right) = rightmost right

ham :: String -> String -> Int
ham s1 s2 =
  if length s1 /= length s2
    then error "Error 43"
    else recursiveHam s1 s2 0

recursiveHam :: String -> String -> Int -> Int
recursiveHam [] [] acc = acc
recursiveHam (x:xs) (y:ys) acc =
  if x == y
    then recursiveHam xs ys acc
    else recursiveHam xs ys acc + 1

trimLeadingZeroes :: String -> String
trimLeadingZeroes [] = []
trimLeadingZeroes s@(x:xs) =
  if x == '0'
    then trimLeadingZeroes xs
    else s

data Cardinal = Cardinal
  { numZeroes :: Int
  , name :: String
  }

--Must be descending order
clist = [Cardinal 6 "Million", Cardinal 3 "Thousand", Cardinal 2 "Hundered"]

wrds :: Integer -> [String]
wrds numbah = recursiveWords (show numbah)

-- tens have no cardinality. Numbers aren't read as "Three twenties or Two Forties" so I'm treating them separately :(
tens :: Char -> String
tens digit =
  case digit of
    '2' -> "Twenty"
    '3' -> "Thirty"
    '4' -> "Fourty"
    '5' -> "Fifty"
    '6' -> "Sixty"
    '7' -> "Seventy"
    '8' -> "Eighty"
    '9' -> "Ninety"

recursiveWords :: String -> [String]
recursiveWords number =
  case number of
    "" -> []
    "0" -> ["Zero"]
    "1" -> ["One"]
    "2" -> ["Two"]
    "3" -> ["Three"]
    "4" -> ["Four"]
    "5" -> ["Five"]
    "6" -> ["Six"]
    "7" -> ["Seven"]
    "8" -> ["Eight"]
    "9" -> ["Nine"]
    "10" -> ["Ten"]
    "11" -> ["Eleven"]
    "12" -> ["Twelve"]
    "13" -> ["Thirteen"]
    "14" -> ["Fourteen"]
    "15" -> ["Fifteen"]
    "16" -> ["Sixteen"]
    "17" -> ["Seventeen"]
    "18" -> ["Eighteen"]
    "19" -> ["Nineteen"]
    _ ->
      if length number == 2
        then tens (head number) : recursiveWords (trimLeadingZeroes [number !! 1])
        else let len = length number
                 (Cardinal nZeroes name) = fbsc clist len
                 splitPos = len - nZeroes
                 x = take splitPos number
                 y = drop splitPos number
             in recursiveWords x ++ [name] ++ recursiveWords (trimLeadingZeroes y)

-- Find biggest(first) cardinal smaller than our number
fbsc :: [Cardinal] -> Int -> Cardinal
fbsc (x@(Cardinal value name):xs) length
  | length > value = x
  | otherwise = fbsc xs length