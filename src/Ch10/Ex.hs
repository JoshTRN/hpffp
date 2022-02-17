module Ch10.Ex where

import Data.Time
    ( fromGregorian, secondsToDiffTime, UTCTime(UTCTime) )

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

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate [] = []
filterDbDate (x : xs) = y ++ filterDbDate xs
  where
    y =
      case x of
        DbDate z -> [z]
        _ -> []

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber [] = []
filterDbNumber (x : xs) = y ++ filterDbNumber xs
  where
    y =
      case x of
        DbNumber z -> [z]
        _ -> []

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb x = fromIntegral (sumDb x) / fromIntegral (length (filterDbNumber x))

