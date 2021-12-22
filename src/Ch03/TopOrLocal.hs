module Ch03.TopOrLocal where

topLevelFunction :: Integer -> Integer
topLevelFunction x =
  x + woot + topLevelValue
  where
    woot = 10 :: Integer

topLevelValue :: Integer
topLevelValue = 5