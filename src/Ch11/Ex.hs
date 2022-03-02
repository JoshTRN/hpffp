{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

import GHC.Float (expts10)
import Data.Int ( Int8 )
import Data.Time (parseTimeOrError)
import Data.List (nub)
import Control.Arrow (ArrowChoice(right))
import Data.Char (isUpper, isLower, ord, chr)

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


data Quantum
  = Yes
  | No
  | Both
  deriving (Eq, Show)

quantSum1 :: Either Quantum Quantum
quantSum1 = Right Yes

quantSum2 :: Either Quantum Quantum
quantSum2 = Right No

quantSum3 :: Either Quantum Quantum
quantSum3 = Right Both

quantSum4 :: Either Quantum Quantum
quantSum4 = Left Yes

quantSum5 :: Either Quantum Quantum
quantSum5 = Left No

quantSum6 :: Either Quantum Quantum
quantSum6 = Left Both

quantProd1 :: (Quantum, Quantum)
quantProd1 = (Yes, Yes)

quantProd2 :: (Quantum, Quantum)
quantProd2 = (Yes, No)

quantProd3 :: (Quantum, Quantum)
quantProd3 = (Yes, Both)

quantProd4 :: (Quantum, Quantum)
quantProd4 = (No, Yes)

quantProd5 :: (Quantum, Quantum)
quantProd5 = (No, No)

quantProd6 :: (Quantum, Quantum)
quantProd6 = (No, Both)

quantProd7 :: (Quantum, Quantum)
quantProd7 = (Both, Yes)

quantProd8 :: (Quantum, Quantum)
quantProd8 = (Both, No)

quantProd9 :: (Quantum, Quantum)
quantProd9 = (Both, Both)

quantFlip1 :: Quantum -> Quantum
quantFlip1 Yes = Yes
quantFlip1 No = Yes
quantFlip1 Both = Yes

quantFlip2 :: Quantum -> Quantum
quantFlip2 Yes = Yes
quantFlip2 No = Yes
quantFlip2 Both = No

quantFlip3 :: Quantum -> Quantum
quantFlip3 Yes = Yes
quantFlip3 No = Yes
quantFlip3 Both = Both

quantFlip4 :: Quantum -> Quantum
quantFlip4 Yes = Yes
quantFlip4 No = No
quantFlip4 Both = Yes

quantFlip5 :: Quantum -> Quantum
quantFlip5 Yes = Yes
quantFlip5 No = No
quantFlip5 Both = No

quantFlip6 :: Quantum -> Quantum
quantFlip6 Yes = Yes
quantFlip6 No = No
quantFlip6 Both = Both

quantFlip7 :: Quantum -> Quantum
quantFlip7 Yes = No
quantFlip7 No = Yes
quantFlip7 Both = Yes

convert1 :: Quantum -> Bool
convert1 Yes = True
convert1 No = True
convert1 Both = True

convert2 :: Quantum -> Bool
convert2 Yes = True
convert2 No = True
convert2 Both = False

convert3 :: Quantum -> Bool
convert3 Yes = True
convert3 No = False
convert3 Both = True

convert4 :: Quantum -> Bool
convert4 Yes = True
convert4 No = False
convert4 Both = False

convert5 :: Quantum -> Bool
convert5 Yes = False
convert5 No = True
convert5 Both = True

convert6 :: Quantum -> Bool
convert6 Yes = False
convert6 No = True
convert6 Both = False

convert7 :: Quantum -> Bool
convert7 Yes = False
convert7 No = False
convert7 Both = True

convert8 :: Quantum -> Bool
convert8 Yes = False
convert8 No = False
convert8 Both = False

data Quad
  = One
  | Two
  | Three
  | Four
  deriving (Eq, Show)

eQuad :: Either Quad Quad
eQuad = undefined -- 8 possibilities

prodQuad :: (Quad, Quad)
prodQuad = undefined -- 16 possibilities

funcQuad :: Quad -> Quad
funcQuad = undefined -- 256 possibilities

prodTBool :: (Bool, Bool, Bool)
prodTBool = undefined -- 8 possibilities

gTwo :: Bool -> Bool -> Bool
gTwo = undefined -- 16 possibilities

fTwo :: Bool -> Quad -> Quad
fTwo = undefined -- 65536 posssibilities

data Silly a b c d
  = MkSilly a b c d
  deriving (Show)

data List a = Nil | Cons a (List a)

data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
  Node
    (Node Leaf 3 Leaf)
    1
    (Node Leaf 4 Leaf)


mapExpected :: BinaryTree Integer
mapExpected =
  Node
    (Node Leaf 4 Leaf)
    2
    (Node Leaf 5 Leaf)

mapOkay :: IO ()
mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "yup OK!"
  else error "test failed!"

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = preorder left ++ [a] ++ preorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = preorder left ++ preorder right ++ [a]

testTree :: Num a => BinaryTree a
testTree =
  Node
    (Node Leaf 1 Leaf)
    2
    (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "Bad news bears"

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ x Leaf = x
foldTree f x (Node left a right) = foldTree f (foldTree f (f a x) left) right

-- mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
-- mapTree _ Leaf = Leaf
-- mapTree f (Node left a right) =
--   Node (mapTree f left) (f a) (mapTree f right)
data Weekday
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday

f :: Show a => (a, b) -> IO (a, b)
f t@(a, _) = do
  print a
  return t

g :: [a] -> a
g xs = xs !! (length xs - 1)

vigenere :: String -> String
vigenere = undefined

-- ceaser :: Int -> String -> String
-- ceaser 0 x = x
-- ceaser _ "" = ""
-- ceaser shift str = map cipherChar str
--   where
--     cipherChar x
--       | isUpper x = change x (ord 'Z') shift
--       | isLower x = change x (ord 'z') shift
--       | otherwise = x
--       where
--         change char last shift
--           | (ord char + shift) > last = chr (ord char - 26 + shift)
--           | otherwise = chr (ord char + shift)

