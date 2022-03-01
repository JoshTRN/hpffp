{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

import GHC.Float (expts10)
import Data.Int ( Int8 )
import Control.Exception (ErrorCall(ErrorCallWithLocation))
import Data.Time (parseTimeOrError)
import Data.List (nub)

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

data Example1
  = Example1 Int
  deriving (Eq, Show)

data Example2 =
  Example2 Int String
  deriving (Eq, Show)

data MyType
  = MyVal Int
  deriving (Eq, Show)

data Example
  = MakeExample Int
  deriving (Show)

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

-- instance (Num a, TooMany a) => TooMany (a, a) where
--   tooMany (x, y) = x + y == 14

-- instance TooMany Goats where
--   tooMany (Goats n) = n > 43

data BigSmall
  = Big Bool
  | Small Bool
  deriving (Eq, Show)


data NumberOrBool
  = Numba Int8
  | BoolyBool Bool
  deriving (Eq, Show)

myNumber :: NumberOrBool
myNumber = Numba (-128)

data QuantumBool
  = QuantumTrue
  | QuantumFalse
  | QuantumBoth
  deriving (Eq, Show)

data TwoQs
  = MkTwoQs QuantumBool QuantumBool
  deriving (Eq, Show)

type TwoQs' = (QuantumBool, QuantumBool)

data Person'
  = MkPerson String Int
  deriving (Eq, Show)

jm :: Person
jm = Person "julie" 108

ca :: Person
ca = Person "chris" 16

namae :: Person' -> String
namae (MkPerson s _ ) = s

data Person = Person
  { name :: String,
    age :: Int
  }
  deriving (Eq, Show)

data Fiction = Fiction deriving Show
data Nonfiction = Nonfiction deriving Show

data BookType
  = FictionBook Fiction
  | NonfictionBook Nonfiction
  deriving (Show)

type AuthorName = String

-- data Author = Author (AuthorName, BookType)


-- data Author
--   = Fiction AuthorName
--   | Nonfiction AuthorName
--   deriving (Eq, Show)


data Expr
  = Number Int
  | Add Expr Expr
  | Minus Expr
  | Mult Expr Expr
  | Divide Expr Expr

-- type Expr =
--   Either Number
--    (Either Add
--      (Either Minus
--        (Either Mult Divide)))

type Number = Int
type Add = (Expr, Expr)
type Minus = Expr
type Mult = (Expr, Expr)
type Divide = (Expr, Expr)

data FlowerType
  = Gardenia
  | Daisy
  | Rose
  | Lilac
  deriving (Show)

type Gardener = String

data Garden = Garden Gardener FlowerType deriving Show

data GuessWhat = ChickenButt deriving (Eq, Show)

data Id a = MkId a deriving (Eq, Show)

data Product a b = Product a b deriving (Eq, Show)

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

data RecordProduct a b = RecordProduct
  { pfirst :: a,
    psecond :: b
  }
  deriving (Eq, Show)

newtype NumCow = NumCow Int deriving (Eq, Show)

newtype NumPig = Numpig Int deriving (Eq, Show)

data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

newtype NumSheep = NumSheep Int deriving (Eq, Show)

data BigFarmhouse = BigFarmhouse NumCow NumPig NumSheep

type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)

type Name = String
type Age = Int
type LovesMud = Bool

type PoundsOfWool = Int

data CowInfo = CowInfo Name Age deriving (Eq, Show)

data PigInfo = PigInfo Name Age LovesMud deriving (Eq, Show)

data SheepInfo = SheepInfo Name Age PoundsOfWool deriving (Eq, Show)

data Animal
  = Cow CowInfo
  | Pig PigInfo
  | Sheep SheepInfo
  deriving (Eq, Show)


type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)

trivialValue :: GuessWhat
trivialValue = ChickenButt


idInt :: Id Integer
idInt = MkId 10

type Awesome = Bool

person :: Product Name Awesome
person = Product "Simon" True

-- data Twitter = Twitter deriving (Eq, Show)
-- data AskFm = AskFm deriving (Eq, Show)

-- socialNetwork :: Sum Twitter AskFm
-- socialNetwork = First Twitter

data SocialNetwork
  = Twitter
  | AskFm
  deriving (Eq, Show)

type Twitter = String
type AskFm = String

twitter :: Sum Twitter AskFm
twitter = First "Twitter"

askFm :: Sum Twitter AskFm
askFm = First "AskFm"

myRecord :: RecordProduct Integer Float
myRecord =
  RecordProduct
    { pfirst = 42,
      psecond = 0.00001
    }
data OperatingSystem
  = GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDS
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgLang
  = Haskell
  | Agda
  | Idris
  | PureScript
  | Lisp
  deriving (Eq, Show)

data Programmer = Programmer
  { os :: OperatingSystem,
    lang :: ProgLang
  }
  deriving (Eq, Show)


nineToFive :: Programmer
nineToFive =
  Programmer
    { os = Mac,
      lang = Haskell
    }
feelingWizardly :: Programmer
feelingWizardly =
  Programmer
    { lang = Agda,
      os = GnuPlusLinux
    }

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux,
    OpenBSDPlusNevermindJustBSDS,
    Mac,
    Windows
  ]

allLanguages :: [ProgLang]
allLanguages =
  [ Haskell,
    Agda,
    Idris,
    PureScript,
    Lisp
  ]

allProgrammers :: [Programmer]
allProgrammers = [ Programmer {os = a, lang = b} | a <- allOperatingSystems, b <- allLanguages]

-- [ Programmer {os = GnuPlusLinux, lang = Haskell},
--   Programmer {os = GnuPlusLinux, lang = Agda},
--   Programmer {os = GnuPlusLinux, lang = Idris},
--   Programmer {os = GnuPlusLinux, lang = PureScript},
--   Programmer {os = OpenBSDPlusNevermindJustBSDS, lang = Haskell},
--   Programmer {os = OpenBSDPlusNevermindJustBSDS, lang = Agda},
--   Programmer {os = OpenBSDPlusNevermindJustBSDS, lang = Idris},
--   Programmer {os = OpenBSDPlusNevermindJustBSDS, lang = PureScript},
--   Programmer {os = Mac, lang = Haskell},
--   Programmer {os = Mac, lang = Agda},
--   Programmer {os = Mac, lang = Idris},
--   Programmer {os = Mac, lang = PureScript},
--   Programmer {os = Windows, lang = Haskell},
--   Programmer {os = Windows, lang = Agda},
--   Programmer {os = Windows, lang = Idris},
--   Programmer {os = Windows, lang = PureScript}
-- ]

-- same as allProgrammers
allProgrammers' :: [Programmer]
allProgrammers' = concatMap (`pair` allOperatingSystems) allLanguages
  where
    pair lang = map (\a -> Programmer { os = a, lang = lang})

-- same as allProgrammers and allProgrammers'
allProgrammers'' :: [Programmer]
allProgrammers'' = go allLanguages
  where
    go [] = []
    go (x:xs) = pair x allOperatingSystems ++ go xs
    pair lang = map (\a -> Programmer { os = a, lang = lang})

data ThereYet
  = There Float Int Bool
  deriving (Eq, Show)

nope :: Float -> Int -> Bool -> ThereYet
nope = There

notYet :: Int -> Bool -> ThereYet
notYet = nope 25.5

notQuite :: Bool -> ThereYet
notQuite = notYet 10

yusssss :: ThereYet
yusssss = notQuite False

newtype Name' = Name' String deriving Show
newtype Acres = Acres Int deriving Show

data FarmerType
  = DairyFarmer
  | WheatFarmer
  | SoyBeanFarmer
  deriving (Show)

data Farmer
  = Farmer Name' Acres FarmerType
  deriving Show

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

data FarmerRec = FarmerRec
  { name' :: Name,
    acres :: Acres,
    farmerType :: FarmerType
  }
  deriving (Show)

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer =
  case farmerType farmer of
    DairyFarmer -> True
    _ -> False

-- bad example, don't do this:
-- data Automobile = Null
--   | Car' {make :: String
--         , model :: String
--         , year :: Integer}
--   deriving Show

data Car' = Car'
  { make :: String,
    model :: String,
    year :: Integer
  }
  deriving (Eq, Show)

data Automobile = Null
  | Automobile Car'
  deriving (Eq, Show)

