module Data.String.Strip
  ( rvrse
  ) where

import Data.Char

strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

notarea :: a -> String -> String
notarea r = strip

rvrse :: [a] -> [a]
rvrse l = rev l []

rev :: [a] -> [a] -> [a]
rev left rvrsed =
  if null left
    then rvrsed
    else rev (tail left) (head left : rvrsed)

test :: (Eq a) => a -> a -> Bool
test x y = x /= y