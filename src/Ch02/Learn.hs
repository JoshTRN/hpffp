module Ch02.Learn where

x :: Integer
x = 10 * 5 + y

myResult :: Integer
myResult = x * 5

y :: Integer
y = 10

foo :: Num a => a -> a
foo x =
  let y = x * 2
      z = x ^ 2
   in 2 * y * z
