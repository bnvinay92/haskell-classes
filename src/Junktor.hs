module Junktor where

class Junktor j where
  jmap :: (a -> b) -> j a -> j b
  (<$) :: a -> j b -> j a
  (<$) = jmap . const

-- Maybe
instance Junktor Maybe where
  jmap f (Just a) = Just (f a)
  jmap _ _        = Nothing

-- UselessBox
newtype Identity a =
  Identity a

instance Junktor Identity where
  jmap f (Identity x) = Identity $ f x

-- Either
instance Junktor (Either a) where
  jmap f (Right b) = Right (f b)
  jmap f (Left y)  = Left y

-- Function
instance Junktor ((->) a) where
  jmap f g = f . g

-- Pair
instance Junktor ((,) a) where
  jmap f (a, b) = (a, f b)

-- Set
data Set a
  = Leaf
  | Node (Set a)
         a
         (Set a)

instance Junktor Set where
  jmap f (Node left v right) = Node (jmap f left) (f v) (jmap f right)
  jmap f _                   = Leaf

-- List
data Gist a
  = Tail
  | Head a
         (Gist a)

instance Junktor Gist where
  jmap f (Head v tail) = Head (f v) (jmap f tail)
  jmap f _             = Tail

