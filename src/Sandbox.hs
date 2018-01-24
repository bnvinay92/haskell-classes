module Sandbox where

import           Control.Applicative

newtype NumMonad a =
  NumMonad a

class Applicative g =>
      Gonad g where
  bind :: g a -> (a -> g b) -> g b

newtype MaybeGonad a =
  MaybeGonad (Maybe a)

instance Functor MaybeGonad where
  fmap f (MaybeGonad m) = MaybeGonad (fmap f m)

instance Applicative MaybeGonad where
  pure = MaybeGonad . Just
  (MaybeGonad f) <*> (MaybeGonad g) = MaybeGonad (f <*> g)

instance Gonad MaybeGonad where
  bind (MaybeGonad (Just a)) f = f a
  bind _ _                     = MaybeGonad Nothing

newtype Box a =
  Box a

instance Functor Box where
  fmap f (Box a) = Box (f a)

guess [] = []
guess (x:xs) = guess [y | y <- xs, y < x] ++ [x] ++ guess [y | y <- xs, y >= x]

nats = 1 : map (+ 1) nats

sieve xx =
  let first = head xx
  in first : sieve (filter (notdivides first) (tail xx))
  where
    notdivides n x = x `mod` n /= 0


--
t1 = Just 10

t2 = Nothing

t3 = Just 20

t4 = []

t5 = [2]

t6 = [5, 3, 8]

t7 = [9, 3]
-- What are the outputs to these?
