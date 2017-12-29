module HigherOrderSpag where

map' f []     = []
map' f (x:xs) = f x : map f xs

filter' pred filtered [] = filtered
filter' pred filtered (x:xs)
  | pred x = filter' pred (filtered ++ [x]) xs
  | otherwise = filter' pred filtered xs

rfold :: (a -> b -> b) -> b -> [a] -> b
rfold _ acc []     = acc
rfold f acc (x:xs) = f x (rfold f acc xs)

lfold :: (b -> a -> b) -> b -> [a] -> b
lfold _ acc [] = acc
lfold f acc (x:xs) = lfold f (f acc x) xs
