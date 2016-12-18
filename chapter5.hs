{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineType where

functionH :: [a] -> a
functionH (x:_) = x

functionC :: Ord a => a -> a -> Bool
functionC x y = if (x > y) then True else False

functionS :: (a, b) -> b
functionS (x, y) = y

i :: a -> a
i a = a

c :: a -> b -> a
c a b = a

c'' :: b -> a -> b
c'' a b = a

c' :: a -> b -> b
c' a b = b

r :: [a] -> [a]
r (_:b) = b
-- r a = a
-- r a = tail a

co :: (b -> c) -> (a -> b) -> a -> c
co x y z = (x (y z))

a :: (a -> c) -> a -> a
a x y = y

a' :: (a -> b) -> a -> b
a' x y = x y

