module Ch10.Ex where

import Data.Time
  ( UTCTime (UTCTime),
    fromGregorian,
    secondsToDiffTime,
  )
import Data.Foldable (maximumBy)

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x : xs) = f x (foldr f z xs)

xs :: [String]
xs = map show [1 .. 5]

y :: [Char]
y = foldr (\x y -> concat ["(", x, "+", y, ")"]) "0" xs

-- "(1+(2+(3+(4+(5+0)))))"

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ acc [] = acc
foldl' f acc (x : xs) = foldl' f (f acc x) xs

conc :: [[a]] -> [a]
conc = concat

f :: [Char] -> [Char] -> [Char]
f x y = conc ["(", x, "+", y, ")"]

out :: [Char]
out = foldl' f "0" (map show [1 .. 5])

-- "(((((0+1)+2)+3)+4)+5)"

-- foldl (flip (*)) 1 [1..3]
-- foldl (flip (*)) (1 * 1) [2,3]
-- foldl (flip (*)) (1 * 1) [2,3]
-- foldl (flip (*)) ((1 * 1) * 2) [3]
-- foldl (flip (*)) ((1 * 1) * 2) * 3) []
-- ((1 * 1) * 2) * 3))
-- (1 * 2) * 3
-- 2 * 3
-- 6

-- foldr and True [[True, False]]
-- and [True] (foldr and [True, False])

x :: [Char]
x = foldl (flip ((++) . show)) "" [1 .. 5]

data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate
      ( UTCTime
          (fromGregorian 1911 5 1)
          (secondsToDiffTime 34123)
      ),
    DbNumber 9001,
    DbString "Hello, world!",
    DbDate
      ( UTCTime
          (fromGregorian 1921 5 1)
          (secondsToDiffTime 34123)
      ),
    DbNumber 2004
  ]

-- original implementation  was done without list comprehension
-- filterDbDate :: [DatabaseItem] -> [UTCTime]
-- filterDbDate [] = []
-- filterDbDate (x : xs) = y ++ filterDbDate xs
--   where
--     y =
--       case x of
--         DbDate z -> [z]
--         _ -> []


-- filterDbNumber :: [DatabaseItem] -> [Integer]
-- filterDbNumber [] = []
-- filterDbNumber (x : xs) = y ++ filterDbNumber xs
--   where
--     y =
--       case x of
--         DbNumber z -> [z]
--         _ -> []

-- list comprehension implementation done with help from the haskell-beginners discord server
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate xs = [z | DbDate z <- xs]

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber xs = [z | DbNumber z <- xs]

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb x = fromIntegral (sumDb x) / fromIntegral (length (filterDbNumber x))

fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

fibsN :: Int -> Integer
fibsN = (!!) fibs

fibs20 :: [Integer]
fibs20 = take 20 fibs

fibsLess100 :: [Integer]
fibsLess100 = takeWhile (<100) fibs

-- factorial :: Integer -> Integer
-- factorial 0 = 1
-- factorial n = n * factorial (n - 1)

factorials :: [Integer]
factorials = 1 : scanl (*) 1 factorials

-- factorial n  = (take n+1 factorials) !! n

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

tups :: String -> String -> [(Char, Char, Char)]
tups [] _ = []
tups _ [] = []
tups (x:xs) (y:ys) = (x,y,x) : tups [x] ys ++ tups xs (y:ys)


tupsP :: [(Char, Char, Char)]
tupsP = filter (\(x,y,z) -> x == 'p') (tups2 stops vowels)

tups2 :: [c] -> [b] -> [(c, b, c)]
tups2 as bs = [ (a, b, a') | a <- as, b <- bs, a' <- as]

seekritFunc :: Fractional a => String -> a
-- calculates the average word length in a sentence as a string
seekritFunc x =
  sum (map (fromIntegral . length)  (words x))
    / fromIntegral (length (words x))

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = if x then True else myOr xs

myOr2 :: [Bool] -> Bool
myOr2 = foldr (\a b -> if a then True else b) False

myOr3 :: [Bool] -> Bool
myOr3 = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a b -> f a || b) False 

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f x = myOr (map f x)

or' :: Bool -> Bool -> Bool
or' a b = if a then a else b

and' :: Bool -> Bool -> Bool
and' a b = if a then b else a

myElem :: Eq a => a -> [a] -> Bool
myElem a = myAny (==a)

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = foldr (\a b -> a == x || b) False

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myReverse' :: [a] -> [a]
myReverse' = foldr (\a b -> b ++ [a]) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a b -> f a : b) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\a b -> f a ++ b ) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "empty list"
myMaximumBy f (x:xs) = foldl (\a b -> if f a b == LT then b else a) x xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "empty list"
myMinimumBy f (x:xs) = foldl (\a b -> if f a b == GT then b else a) x xs
