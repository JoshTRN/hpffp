module Ch05.TypeInference2 where

f x y = x + y + 3

myConcat :: [Char] -> [Char]
myConcat x = x ++ " yo"

triple :: Integer -> Integer
triple x = tripleItYo x
  where
    tripleItYo :: Integer -> Integer
    tripleItYo y = y * 3

