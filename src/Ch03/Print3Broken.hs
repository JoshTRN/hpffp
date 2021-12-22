module Ch03.Print3Broken where

printSecond :: IO ()
printSecond = do
  putStrLn greeting
  where
    greeting =
      "Yaaaarrrr"

main :: IO ()
main = do
  putStrLn greeting
  printSecond
  where
    greeting = "Yaaaaarrr"