module Ch06.Ex where

data DayOfWeek
  = Mon
  | Tues
  | Weds
  | Thur
  | Fri
  | Sat
  | Sun
  deriving (Show)

data Date
  = Date DayOfWeek Int
  deriving (Show)

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tues Tues = True
  (==) Weds Weds = True
  (==) Thur Thur = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False

instance Eq Date where
  (==)
    (Date weekday dayOfMonth)
    (Date weekday' dayOfMonth') =
      weekday == weekday' && dayOfMonth == dayOfMonth'


data Identity a = Identity a

-- instance Eq a => Eq (Identity a) where
--   (==) (Identity v) (Identity v') = v == v'


data NoEq = NoEqInst deriving Show

instance Ord a => Eq (Identity a) where
  (==) (Identity v) (Identity v') =
    compare v v' == EQ

data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn a) (TisAn a') = True

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two a b) (Two a' b') =
    a == a' && b == b'

data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair a b) (Pair a' b')
    = a == a' && b == b'

data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple a' b') = a == a && b == b

data Which a
  = ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne a) (ThisOne a') = a == a'
  (==) (ThatOne a) (ThatOne a') = a == a'
  (==) _ _ = False

