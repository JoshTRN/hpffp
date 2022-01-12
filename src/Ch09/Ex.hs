module Ch09.Ex where

import Data.Bool (bool)
import Data.Char (chr, isLower, isUpper, ord, toUpper)
import GHC.Base (NonEmpty)

myHead :: [a] -> Maybe a
myHead [] = Nothing
myHead (x : _) = Just x

myTail :: [a] -> Maybe [a]
myTail [] = Nothing
myTail [_] = Nothing
myTail (_ : xs) = Just xs

eftBool :: Bool -> Bool -> [Bool]
eftBool x y =
  case compare x y of
    GT -> []
    _ -> [x, y]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd x y =
  case compare x y of
    GT -> []
    _ -> [x, y]

eftInt :: Int -> Int -> [Int]
eftInt x y = go x y []
  where
    go x y z
      | x > y = []
      | x == y = z ++ [y]
      | otherwise = go (succ x) y (z ++ [x])

eftChar :: Char -> Char -> [Char]
eftChar x y = go x y []
  where
    go x y z
      | x > y = []
      | x == y = z ++ [y]
      | otherwise = go (succ x) y (z ++ [x])

listOfWords :: String -> [String]
listOfWords x = go (dropWhile (== ' ') x) []
  where
    go x y
      | word == "" = y
      | otherwise = go rest (y ++ [word])
      where
        word = takeWhile (/= ' ') x
        rest = dropWhile (== ' ') (dropWhile (/= ' ') x)

firstSen :: [Char]
firstSen = "Tyger Tyger, burning bright\n"

secondSen :: [Char]
secondSen = "In the forests of the night\n"

thirdSen :: [Char]
thirdSen = "What immortal hand or eye\n"

fourthSen :: [Char]
fourthSen =
  "Could frame thy fearful\
  \ symmetry?"

sentences :: [Char]
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

sepByChar :: Char -> String -> [String]
sepByChar char x = go (dropWhile (== char) x) []
  where
    go x y
      | word == "" = y
      | otherwise = go rest (y ++ [word])
      where
        word = takeWhile (/= char) x
        rest = dropWhile (== char) (dropWhile (/= char) x)

shouldEqual :: [String]
shouldEqual =
  [ "Tyger Tyger, burning bright",
    "In the forests of the night",
    "What immortal hand or eye",
    "Could frame thy fearful symmetry?"
  ]

testSeparator :: IO ()
testSeparator =
  print $
    if sepByChar '\n' sentences == shouldEqual
      then "They are equal"
      else "They are not equal"

length' :: [a] -> Integer
length' [] = 0
length' (_ : xs) = 1 + length' xs

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x : xs) = x + mySum xs

-- map (\x -> if x == 3 then (-x) else (x)) [1..10]

something :: [Integer]
something = map (\x -> bool x (negate x) (x == 3)) [1 .. 10]

multiplesOf3 :: [Int] -> [Int]
multiplesOf3 = filter (\x -> rem x 3 == 0)

lenMult3 :: [Int] -> Int
lenMult3 = length . multiplesOf3

filterArticles :: String -> [String]
filterArticles = filter (\x -> x /= "the" && x /= "a") . words

myZip :: [a] -> [b] -> [(a, b)]
myZip = myZipWith (,)

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f [] _ = []
myZipWith f _ [] = []
myZipWith f (x : xs) (y : ys) = f x y : myZipWith f xs ys

hello :: [Char]
hello = filter isUpper "HbEfLrLxO"

cap :: [Char] -> [Char]
cap [] = []
cap (x : xs) = toUpper x : xs

recap :: String -> String
recap "" = ""
recap (x : xs) = toUpper x : recap xs

first :: [Char] -> Char
first = toUpper . head

ceaser :: Int -> String -> String
ceaser 0 x = x
ceaser _ "" = ""
ceaser shift str = map cipherChar str
  where
    cipherChar x
      | isUpper x = change x (ord 'Z') shift
      | isLower x = change x (ord 'z') shift
      | otherwise = x
      where
        change char last shift
          | (ord char + shift) > last = chr (ord char - 26 + shift)
          | otherwise = chr (ord char + shift)

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x : xs) = if x then x else myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = myOr . map f

myReverse :: [a] -> [a]
myReverse [] = []
myReverse x = go x []
  where
    go [] _ = []
    go (x : xs) y
      | null xs = x : y
      | otherwise = go xs (x : y)

squish :: [[a]] -> [a]
squish [] = []
squish (x : xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x : xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "cannot take maximum of empty list"
myMaximumBy f [x] = x
myMaximumBy f (x : xs)
  | length xs == 1 = compare f x (head xs)
  | otherwise = myMaximumBy f (compare f x (head xs) : tail xs)
  where
    compare f x y =
      case f x y of
        LT -> y
        _ -> x

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy = myMaximumBy . flip

-- List and Cons example
data List a = Nil | Cons a (List a)
