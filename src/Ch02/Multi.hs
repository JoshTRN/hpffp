module Ch02.Multi where

mult1 :: Integer
mult1 = x * y
  where
    x = 5
    y = 6

waxOn :: Integer
waxOn = x * 5
  where
    z = 7
    y = z + 8
    x = y ^ 2

triple :: Num a => a -> a
triple x = x * 3

waxOff :: Num a => a -> a
waxOff = triple
