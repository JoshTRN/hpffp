module Ch06.Enum where

class Numberish a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer
  defaultNumber :: a

newtype Age =
  Age Integer
  deriving (Eq, Show)

instance Numberish Age where
  fromNumber n = Age n
  toNumber (Age n) = n
  defaultNumber = Age 65

newtype Year
  = Year Integer
  deriving (Eq, Show)

instance Numberish Year where
  fromNumber n = Year n
  toNumber (Year n) = n
  defaultNumber = Year 25


sumNumberish :: Numberish a => a -> a -> a
sumNumberish a  a' = fromNumber summed
  where
    integerOfA = toNumber a
    integerOfAPrime = toNumber a'
    summed = integerOfA + integerOfAPrime


addWeird :: (Ord a, Num a) => a -> a -> a
addWeird x y =
  if x > 1
  then x + y
  else x

data Person = Person Bool deriving (Show)

printPerson :: Person -> IO ()
printPerson = print

data Mood
  = Blah
  | Woot
  deriving Show

instance Eq Mood where
  Blah == Blah = True
  Woot == Woot = False
  _ == _ = False
  
settleDown :: Mood -> Mood
settleDown x = if x == Woot
  then Blah
  else x

type Subject = String
type Verb = String
type Object = String

data Sentence
  = Sentence Subject Verb Object
  deriving Show

s1 :: Object -> Sentence
s1 = Sentence "dogs" "drool"

s2 :: Sentence
s2 = Sentence "Julie" "loves" "dogs"

data Rocks = Rocks String deriving (Ord, Eq, Show) 

data Yeah = Yeah Bool deriving (Ord, Eq, Show)

data Papu = Papu Rocks Yeah deriving (Ord, Eq, Show)

phew :: Papu
phew = Papu (Rocks "chases") (Yeah True)

equalityForAll :: Papu -> Papu -> Bool 
equalityForAll p p' = p == p'

comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'

i :: Num a => a
i = 1

f :: RealFrac a => a
f = 1.0


freud :: Ord a => a -> a
freud x = x

freud' :: Int -> Int
freud' x = x

myX :: Int
myX = 1 :: Int

sigmund :: Int -> Int
sigmund x = myX
