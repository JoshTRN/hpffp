{-# LANGUAGE NoMonomorphismRestriction #-}

module Ch05.DetermineType where

example :: Num p => p
example = 1

functionH :: [a] -> a
functionH (x : _) = x

functionS :: (x, y) -> y
functionS (x, y) = y

functionC :: Ord a => a -> a -> Bool
functionC x y =
  if (x > y) then True else False

co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB a = bToC $ aToB a

a :: (a -> c) -> a -> a
a _ x = x

a' :: (a -> b) -> a -> b
a' aToB = aToB
