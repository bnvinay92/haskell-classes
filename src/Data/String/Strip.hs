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

data List a = List a (List a)| EmptyList deriving Show

list = List 1 (List 2 (List 3 EmptyList))

lst :: List a -> a
lst (List head tail) = case tail of
                       EmptyList -> head
                       List head tail -> lst tail

