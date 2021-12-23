module Ch04.Ex where

-- 8. Write a function that tells you whether or not a given String (or
-- list) is a palindrome. Here, you’ll want to use a function called
-- reverse, a predefined function that does what it sounds like:

-- 9. Write a function to return the absolute value of a number using
-- an if-then-else expression:

myAbs :: Integer -> Integer
myAbs x = if x > 0 then x else x * (-1)

-- 10. Fill in the definition of the following function, using fst and snd:

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))

-- Correcting syntax
-- In the following examples, you’ll be shown syntactically incorrect
-- code. Type it in, and try to correct it in your text editor, validating it
-- with GHC or GHCi.

-- 1. Here, we want a function that adds 1 to the length of a string
-- argument and returns that result:

-- x = (+)
-- F xs = w 'x' 1
-- where w = length xs

x :: Int -> Int -> Int
x = (+)

s :: [p] -> Int
s xs = w `x` 1
  where
    w = length xs

-- 2. This is supposed to be the identity function, id:
-- \X = x

f2 :: p -> p
f2 a = a

-- 3. When fixed, this function will return 1 from the value (1, 2):
-- f (a b) = A

f3 :: (a, b) -> a
f3 = fst

f4 :: [p] -> Int
f4 = s

z :: Bool
z = isPalindrome "racecar"


isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = y == z
  where
    middle = length x `div` 2
    y = take middle x
    z = reverse $ drop (if length x `mod` 2 == 1 then middle + 1 else middle) x
