module Ch03.Ex2 where

ninthOnward :: String -> String
ninthOnward = drop 9

fourthLetter :: String -> String
fourthLetter x = take 1 $ drop 4 x

appendExclamation :: String -> String
appendExclamation x = x ++ "!"

thirdLetter :: Int -> Char
thirdLetter x = "Curry is awesome!" !! x
