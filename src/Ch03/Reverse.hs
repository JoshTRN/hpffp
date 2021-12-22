module Ch03.Reverse where

rvs :: String -> String
rvs str = x ++ y ++ z
  where
    x = drop 9 str
    y = take 4 $ drop 5 str
    z = take 5 str

main :: IO ()
main = print $ rvs "Curry is awesome"