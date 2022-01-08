module Ch08.Ex where
import Data.List (intersperse, intercalate)

-- 8.2 Factorial

brokenFact1 :: Integer -> Integer
brokenFact1 n = n * brokenFact1 (n - 1)

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n = n
incTimes times n = 1 + incTimes (times - 1) n

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f (applyTimes (n -1) f b)

incTimes' :: (Eq a, Num a) => a -> a -> a
incTimes' times = applyTimes times (+ 1)

applyTimes' :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes' 0 f b = b
applyTimes' n f b = f . applyTimes' (n -1) f $ b

-- applytimes 5 (+1) 5
-- (+1) (applyTimes 4 (+1) 5)
-- (+1) ((+1) (applyTimes 3 (+1) 5)))
-- (+1) ((+1) ((+1) (applyTimes 2 (+1) 5)))
-- (+1) ((+1) ((+1) ((+1) (applyTimes 1 (+1) 5))))
-- (+1) ((+1) ((+1) ((+1) ((+1) (applyTimes 0 (+1) 5)))))
-- (+1) ((+1) ((+1) ((+1) ((+1) 5))))

-- 8.4 Fibbonacci

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x -1) + fibonacci (x - 2)

-- fibonacci = (map fibm [0..] !!)
--     where fibm 1 = 0
--           fibm 2 = 1
--           fibm n = fibonacci (n-1) + fibonacci (n-2)

-- 8.5 Integral division from scratch

-- type Numerator = Integer
-- type Denomenator = Integer
-- type Quotient = Integer
-- type Result = Integer


-- dividedBy :: Numerator -> Denomenator -> Result -> Quotient
-- dividedBy 0 y z = z
-- dividedBy x y z = dividedBy (x-y) y (z + 1)

dividedBy :: Integral a => a -> a -> a
dividedBy num denom = go num denom 0
  where
    go n d count
      | n < d = count
      | otherwise = go (n-d) d (count + 1)


cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y
-- fill in the types

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

sumToN :: Int -> Int
sumToN n = go n 0 0
  where
    go n t c
      | c == n = t + n
      | otherwise = go n (t + c) (c + 1)


mult :: Integral a => a -> a -> a
mult x y = go x x y 1
  where
    go o x y z
      | z == y = x
      | otherwise = go o (x + o) y (z + 1)


digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = "only 1 - 9 allowed"


digits :: Int -> [Int]
digits n = go [] n
  where
    go res n
      | x == 0 = y:res
      | otherwise = go (y:res) x
      where
        (x,y) = n `divMod` 10


wordNumber :: Int -> String
wordNumber n = intercalate "-" . map digitToWord $ digits n





