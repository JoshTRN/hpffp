{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

import GHC.Float (expts10)

data PugType = PugData
--     [1]       [2]
-- 1. Takes no arguments, so we can consider it a type constant
-- 2. Single data constructor because it takes no arguments. If A function requires a PugType, you know it will be pug data.

data HuskyType a = HuskyData
--      [3]          [4]
-- 3 HuskyType is the type constructor and it takes a single polymorphic argument.
-- 4. The type variable argument does _not_ occur as an argument to HuskyData, which makes it a phantom.
--    Husky data is a constant value like PugData

data DogueDeBordeaux doge = DogueDeBordeaux doge
--      [5]          [6]


myPug :: PugType
myPug = PugData :: PugType

myHusky :: HuskyType a
myHusky = HuskyData

myOtherHusky :: Num a => HuskyType a
myOtherHusky = HuskyData

myOtherOtherHusky :: HuskyType [[[[Int]]]]
myOtherOtherHusky = HuskyData


myDoge :: DogueDeBordeaux Int
myDoge = DogueDeBordeaux 10


data Doggies a
  = Husky a
  | Mastiff a
  deriving (Eq, Show)

-- Doggies
data Price = Price Integer deriving (Eq, Show)

data Manufacturer
  = Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data Airline
  = PapuAir
  | CatapultsR'Us
  | TakeYourChanesUnited
  deriving (Eq, Show)

data Vehicle
  = Car Manufacturer Price
  | Plane Airline Size
  deriving (Eq, Show)

data Size = Size Integer deriving (Eq, Show)

myCar :: Vehicle
myCar = Car Mini (Price 14000)

urCar :: Vehicle
urCar = Car Mazda (Price 20000)

clownCar :: Vehicle
clownCar = Car Tata (Price 7000)

doge :: Vehicle
doge = Plane PapuAir (Size 1000)

isCar :: Vehicle -> Bool
isCar x = case x of
  Car _ _ -> True
  _ -> False

isPlane :: Vehicle -> Bool
isPlane x = case x of
  Plane _ _ -> True
  _ -> False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car x _) = x

data Example0 =
  Example0
  deriving (Eq, Show)

data Example1 =
  Example1 Int
  deriving (Eq, Show)

data Example2 =
  Example2 Int String
  deriving (Eq, Show)

data MyType = MyVal Int
  deriving (Eq, Show)

data Example = MakeExample Int deriving Show

-- data Goats = Goats Int deriving (Eq, Show)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42


newtype Goats = Goats Int deriving (Eq, Show, TooMany)

newtype Cows = Cows Int deriving (Eq, Show)

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany (Int, String) where
  tooMany (x, _) = x > 42

instance TooMany (Int, Int) where
  tooMany (x, y) = x + y > 42

-- instance TooMany (Num a, TooMany a) => (a, a) where
--   tooMany (Num x, TooMany y) = x + y > 42

-- instance TooMany Goats where
--   tooMany (Goats n) = n > 43

