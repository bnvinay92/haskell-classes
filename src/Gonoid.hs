module Gonoid where

import           Data.List      (sort)
import           Data.Map       (Map, fromAscList, intersectionWith, singleton)
import           Data.Semigroup
import           Data.Set       (Set, fromList, intersection)

class Semigroup a =>
      Gonoid a where
  neutral :: a
  gconcat :: [a] -> a
  gconcat = foldr (<>) neutral

-- Maybe
instance Semigroup a => Gonoid (Maybe a) where
  neutral = Nothing

-- BST: Union
data BST a
  = Leaf
  | Node (BST a)
         a
         (BST a)
  deriving (Show)

union x y = btreeFromList $ sort (flatten x ++ flatten y)
  where
    flatten Leaf         = []
    flatten (Node x v y) = flatten x ++ [v] ++ flatten y
    btreeFromList = foldr (Node Leaf) Leaf

instance (Ord a) => Semigroup (BST a) where
  (<>) = union

instance (Ord a) => Gonoid (BST a) where
  neutral = Leaf

-- Map: Intersect
newtype MapGonoid k a =
  MapGonoid (Map k a)
  deriving (Show)

instance (Ord k, Semigroup a) => Semigroup (MapGonoid k a) where
  (MapGonoid a) <> (MapGonoid b) = MapGonoid $ intersectionWith (<>) a b

instance (Ord k, Enum k, Bounded k, Gonoid a) => Gonoid (MapGonoid k a) where
  neutral = MapGonoid $ fromAscList [(k, neutral) | k <- [minBound .. maxBound]]

-- Set: Intersect
newtype SetGonoid a =
  SetGonoid (Set a)
  deriving (Show)

instance Ord a => Semigroup (SetGonoid a) where
  (SetGonoid a) <> (SetGonoid b) = SetGonoid $ intersection a b

instance (Ord a, Enum a, Bounded a) => Gonoid (SetGonoid a) where
  neutral = SetGonoid $ fromList [minBound .. maxBound]

-- List
data ListGonoid a
  = EmptyList
  | Cons a
         (ListGonoid a)
  deriving (Show)

instance Semigroup (ListGonoid a) where
  x <> y = x ++ y
    where
      EmptyList ++ y = y
      (Cons x xs) ++ ys = Cons x (xs ++ ys)

instance Gonoid (ListGonoid a) where
  neutral = EmptyList

-- First
newtype FirstGonoid a =
  FirstGonoid (Maybe a)
  deriving (Show)

instance Semigroup (FirstGonoid a) where
  x@(FirstGonoid (Just i)) <> _ = x
  _ <> y = y

instance Gonoid (FirstGonoid a) where
  neutral = FirstGonoid Nothing

-- Last
newtype LastGonoid a =
  LastGonoid (Maybe a)
  deriving (Show)

instance Semigroup (LastGonoid a) where
  _ <> y@(LastGonoid (Just i)) = y
  x <> _ = x

instance Gonoid (LastGonoid a) where
  neutral = LastGonoid Nothing

-- Min
newtype MinGonoid a =
  MinGonoid a
  deriving (Show)

instance (Ord a, Bounded a) => Semigroup (MinGonoid a) where
  MinGonoid a <> MinGonoid b = MinGonoid $ min a b

instance (Ord a, Bounded a) => Gonoid (MinGonoid a) where
  neutral = MinGonoid minBound

-- Max
newtype MaxGonoid a =
  MaxGonoid a
  deriving (Show)

instance (Ord a, Bounded a) => Semigroup (MaxGonoid a) where
  MaxGonoid a <> MaxGonoid b = MaxGonoid $ max a b

instance (Ord a, Bounded a) => Gonoid (MaxGonoid a) where
  neutral = MaxGonoid minBound

-- Any
newtype AnyGonoid =
  AnyGonoid Bool
  deriving (Show)

instance Semigroup AnyGonoid where
  AnyGonoid x <> AnyGonoid y = AnyGonoid $ x || y

instance Gonoid AnyGonoid where
  neutral = AnyGonoid False

-- All
newtype AllGonoid =
  AllGonoid Bool
  deriving (Show)

instance Semigroup AllGonoid where
  AllGonoid x <> AllGonoid y = AllGonoid $ x && y

instance Gonoid AllGonoid where
  neutral = AllGonoid True

-- Add
newtype AddGonoid a =
  AddGonoid a
  deriving (Show)

instance Num a => Semigroup (AddGonoid a) where
  AddGonoid a <> AddGonoid b = AddGonoid $ a + b

instance Num a => Gonoid (AddGonoid a) where
  neutral = AddGonoid 0

-- Prod
newtype ProdGonoid a =
  ProdGonoid a
  deriving (Show)

instance Num a => Semigroup (ProdGonoid a) where
  ProdGonoid a <> ProdGonoid b = ProdGonoid (a * b)

instance Num a => Gonoid (ProdGonoid a) where
  neutral = ProdGonoid 1
