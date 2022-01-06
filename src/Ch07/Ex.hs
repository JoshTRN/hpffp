import GHC.Float (expts10)
-- module Ch07.Ex where

myNum :: Num a => a
myNum = 1

myVal :: Num a => a -> a
myVal f = f + myNum

stillAFunction :: [a] -> [a] -> [a] -> [a]
stillAFunction a b c = a ++ b ++ c

addOne :: Integer -> Integer
addOne = (+ 1)

bindExp :: Integer -> String
bindExp x =
  let y = 5
  in "the integer was: " ++ show x
        ++ "and y was: "
        ++ show y

addOneIfOdd :: Integral p => p -> p
addOneIfOdd n = if odd n then f n else n
  where
    f = (+ 1)

addFive :: Integer -> Integer -> Integer
addFive x y = (if x > y then y else x) + 5

myFlip :: (t1 -> t2 -> t3) -> t1 -> t2 -> t3
myFlip f x y = f x y

isItTwo :: Integer -> Bool
isItTwo 2 = True
isItTwo _ = False

isItTwo' :: Integer -> Bool
isItTwo' x = x == 2

newtype Username
  = Username String

newtype AccountNumber
  = AccountNumber Integer

data User
  = UnregisteredUser
  | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser
  ( RegisteredUser
      (Username name)
      (AccountNumber acctNum)
    ) =
    putStrLn $ name ++ " " ++ show acctNum

data WherePenguinsLive
  = Galapagos
  | Antarctica
  | Australia
  | SouthAfrica
  | SouthAmerica
  deriving (Eq, Show)

data Penguin =
  Peng WherePenguinsLive
  deriving (Eq, Show)


isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _ = False


gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereItLives) = whereItLives


humboldt :: Penguin
humboldt = Peng SouthAmerica

gentoo :: Penguin
gentoo = Peng Antarctica

macaroni :: Penguin
macaroni = Peng Antarctica

little :: Penguin
little = Peng Australia

galapagos :: Penguin
galapagos = Peng Galapagos


galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _ = False

antarcticPenguin :: Penguin -> Bool
antarcticPenguin (Peng Antarctica) = True
antarcticPenguin _ = False

antarcticOrGalapagos :: Penguin -> Bool
antarcticOrGalapagos p = galapagosPenguin p || antarcticPenguin p

f :: (a, b, c) -> (d, e, f) -> ((a,d), (c,f))
f (a,b,c) (d,e,f) = ((a,d), (c,f))

-- Case Expressions

funcZ :: (Eq a, Num a) => a -> [Char]
funcZ x =
  case x + 1 == 1 of
    True -> "AWESOME"
    False -> "wut"

pal :: Eq a => [a] -> [Char]
pal xs =
  case xs == reverse xs of
    True -> "yes"
    False -> "no"


pal' :: Eq a => [a] -> [Char]
pal' xs =
  case y of
    True -> "yes"
    False -> "no"
  where y = xs == reverse xs


functionC :: Ord p => p -> p -> p
functionC x y =
  case x > y of
    True -> x
    False -> y

ifEvenAdd2 :: Integral p => p -> p
ifEvenAdd2 n =
  case even n of
    True -> n+2
    False -> n

nums :: (Ord a, Num a, Num p) => a -> p
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

-- myFlip :: (a -> b -> c) -> b -> a -> c
-- myFlip f x y = f y x


returnLast :: a -> b -> c -> d -> d
returnLast _ _ _ d = d

returnLast' :: a -> (b -> (c -> (d -> d)))
returnLast' _ _ _ d = d

-- returnBroke :: (((a -> b) -> c) -> d) -> d
-- returnBroke _ _ _ d = d

returnAfterApply :: (a -> b) -> a -> c -> b
returnAfterApply f a c = f a 

data Employee
  = Coder
  | Manger
  | Veep
  | CEO
  deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' =
  putStrLn $ show e ++ " is the boss of " ++ show e'

employeeRank :: (Employee -> Employee -> Ordering) -> Employee -> Employee -> IO ()
employeeRank f e e' =
  case f e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither employee is the boss"
    LT -> reportBoss e' e


codersRuleCEOsDrool :: Employee -> Employee -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _ = GT
codersRuleCEOsDrool _ Coder = LT
codersRuleCEOsDrool e e' = compare e e'


dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Integer -> Integer
oneIsOne = dodgy 1

oneIsTwo :: Integer -> Integer
oneIsTwo = flip dodgy $ 2


  -- 7.7 guards

myAbs :: Integer -> Integer
myAbs x
  | x < 0 = - x
  | otherwise = x

bloodNa :: Integer -> String
bloodNa x
  | x < 135 = "too low"
  | x < 145 = "too high"
  | otherwise = "just right"


isRight :: (Num a, Eq a) => a -> a -> a -> String
isRight a b c
  | a^2 + b^2 == c^2 = "RIGHT ON"
  | otherwise = "not right"


dogYrs :: Integer -> Integer
dogYrs x
  | x <= 0 = 0
  | x <= 1 = x * 15
  | x <= 2 = x * 12
  | x <= 4 = x * 8
  | otherwise = x * 6


avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  | otherwise = 'F'
  where y = x / 100

palin :: Eq a => [a] -> Bool
palin xs
  | xs == reverse xs = True
  | otherwise = False


numbers :: (Num a, Ord a) => a -> Int
numbers x
  | x < 0 = -1
  | x == 0 = 0
  | otherwise = 1


-- 7.8 Function Composition


filter2 :: (a -> Bool) -> [a] -> [a]
filter2 f (x:xs) = if f x then  x:filter2 f xs else filter2 f xs
filter2 _ _ = []

-- 7.9 Point-free style

-- f2 = negate . sum

add :: Int -> Int -> Int
add x y = x + y

addPf :: Int -> Int -> Int
addPf = (+)

add1 :: Int -> Int
add1 x = x + 1


add1Pf :: Int -> Int
add1Pf = add 1


 -- 7.10 Demonstrating Composition


tensDigit :: Integral a => a -> a
tensDigit x = snd $ x `divMod` 10

hunsD :: Integral a => a -> a
hunsD x = fst $ y `divMod` 100
  where (_,y) = x `divMod` 1000

foldBool :: a -> a -> Bool -> a 
foldBool x y z
  | z = y
  | otherwise = x

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (b, c)
  where
    b = f a

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTrip' :: (Show a, Read a) => a -> a
roundTrip' = read . show

roundTrip'' :: (Show a, Read b) => a -> b
roundTrip'' a = read (show a :: String)

main = do
  print ((roundTrip'' 4) :: Int)
  print (id 4)
