{-# LANGUAGE BangPatterns #-}

module Ch12.Ex where
import Data.Char (toLower)
import Ch10.Ex (xs)
import Data.List (foldl')


ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 n = if even n then Just (n + 2) else Nothing

type Name = String
type Age = Integer

data Person
  = Person Name Age
  deriving Show

-- mkPerson :: Name -> Age -> Maybe Person
-- mkPerson name age
--   | name /= "" && age >= 0 = Just $ Person name age
--   | otherwise = Nothing

-- data Either a b = Left a | Right b

data PersonInvalid
  = NameEmpty
  | AgeTooLow
  -- deriving (Eq, Show)

toString :: PersonInvalid -> String
toString NameEmpty = "NameEmpty"
toString AgeTooLow = "AgeTooLow"

instance Show PersonInvalid where
  show = toString

instance Eq PersonInvalid where
  (==) NameEmpty NameEmpty = True
  (==) AgeTooLow AgeTooLow = True
  (==) _ _ = False

blah :: PersonInvalid -> String
blah pi
  | pi == NameEmpty = "NameEmpty"
  | pi == AgeTooLow = "AgeTooLow"
  | otherwise = "???"

mkPerson :: Name -> Age -> Either [PersonInvalid] Person
mkPerson name age = mkPerson' (nameOkay name) (ageOkay age)

mkPerson' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPerson' (Right nameOk) (Right ageOk) = Right (Person nameOk ageOk)
mkPerson' (Left badName) (Left badAge) = Left (badName ++ badAge)
mkPerson' (Left badName) _ = Left badName
mkPerson' _ (Left badAge) = Left badAge

djali :: Either [PersonInvalid] Person
djali = mkPerson "Djali" 5

type ValidatePerson a = Either [PersonInvalid] a

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age
  | age >= 0 = Right age
  | otherwise = Left [AgeTooLow]


nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name
  | name == "" = Left [NameEmpty]
  | otherwise = Right name


countVowels :: String -> Integer
countVowels x = fromIntegral (length (filter (`elem` "aeiouAEIOU") x))

replaceThe :: String -> String
replaceThe x = unwords . map (\a -> if a == "the" then "a" else a) $ words x


-- countTheBeforeVowel :: String -> Integer
-- countTheBeforeVowel x = go x (toInteger 0) False
--   where
--     nextWords = drop 1 . dropWhile (/=' ')
--     go [] total _ = total
--     go t@(x : xs) total True =
--       go (nextWords t) (if x `elem` "aeiouAEIOU" then total + 1 else total) False
--     go t@(x : xs) total _
--       | x == 't' = checkForThe t 4 "the "
--       --  | x == ' ' = checkForThe t 4 " the "
--       | otherwise = go (nextWords t) total False
--       where
--         checkForThe xs' limit match =
--           if take limit xs' == match
--             then go (drop limit xs') total True
--             else go (nextWords xs') total False


countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = countTheBeforeVowel' 0 . words . map toLower where
  countTheBeforeVowel' !n [] = n
  countTheBeforeVowel' !n ("the":(v:_):rest) | isVowel v = countTheBeforeVowel' (n+1) rest
  countTheBeforeVowel' !n (_: rest) = countTheBeforeVowel' n rest
  isVowel = (`elem` "aiueo")

newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels :: String
vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord x =
  if length (filter (not . (`elem` vowels)) x) > length (filter (`elem` vowels) x)
    then Just $ Word' x
    else Nothing

data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x >= 0 = Just (helper x)
  | otherwise = Nothing
  where
    helper 0 = Zero
    helper x = Succ (helper (x-1))

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b f (Just a) = f a
mayybee b f Nothing = b

fromMaybe :: a -> Maybe a -> a
fromMaybe a (Just a') = a'
fromMaybe a Nothing = a

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes (Just x:xs) = x:catMaybes xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Nothing
flipMaybe (Nothing:xs) = Nothing
flipMaybe t@(x:xs)
  | length (helper t) == length t = Just (helper t)
  | otherwise = Nothing
  where
    helper [] = []
    helper (Just x: xs) = x:helper xs
    helper (Nothing:_) = []

flipMaybe' :: [Maybe a] -> Maybe [a]
flipMaybe' [] = Just []
flipMaybe' (Nothing:_) = Nothing
flipMaybe' (Just x:xs) =
  case flipMaybe' xs of
    Nothing -> Nothing
    Just xs -> Just (x:xs)

lefts' :: [Either a b] -> [a]
lefts' [] = []
lefts' (Left a:as) = a:lefts' as
lefts' (Right b:as) = lefts' as

lefts'' :: [Either a b] -> [a]
lefts'' [] = []
lefts'' as =
  foldr
    ( \cur acc -> case cur of
        Left x -> x : acc
        _ -> acc
    )
    []
    as


rights' :: [Either a b] -> [b]
rights' [] = []
rights' as =
  foldr
    ( \cur acc -> case cur of
        Right x -> x : acc
        _ -> acc
    )
    []
    as

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = go ([], [])
  where
    go (as, bs) [] = (as, bs)
    go (as, bs) (x : xs) =
      case x of
        Left a -> go (as ++ [a], bs) xs
        Right b -> go (as, bs ++ [b]) xs

partitionEithers'' :: [Either a b] -> ([a], [b])
partitionEithers'' =
  foldr
    ( \cur (as, bs) ->
        case cur of
          Left a -> (a:as, bs)
          Right b -> (as, b:bs)
    )
    ([], [])


partitionEithers''' :: [Either a b] -> ([a], [b])
partitionEithers''' = foldr separate ([], [])
    where
      separate (Right b) (as, bs) = (as, b:bs)
      separate (Left a) (as, bs) = (a:as, bs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right b) = Just (f b)
eitherMaybe' f (Left a) = Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ g (Right a) = g a

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)

mehSum :: Num a => [a] -> a
mehSum xs = go 0 xs
  where
    go :: Num a => a -> [a] -> a
    go n [] = n
    go n (x:xs) = go (n + x) xs

-- niceSum :: num a -> [a] -> a
-- niceSum = foldl' (+) 0

mehProduct :: Num a => [a] -> a
mehProduct xs = go 1 xs
  where
    go :: Num a => a -> [a] -> a
    go n [] = n
    go n (x:xs) = go (n * x) xs

niceProduct :: Num a => [a] -> a
niceProduct = foldl' (*) 1

mehConcat :: [[a]] -> [a]
mehConcat xs = go [] xs
  where
    go :: [a] -> [[a]] -> [a]
    go xs' [] = xs'
    go xs' (x:xs) = go (xs' ++ x) xs

niceConcat :: [[a]] -> [a]
niceConcat = concat

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x: myIterate f (f x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b =
  case f b of
   Nothing -> []
   Just (a, b) -> a:myUnfoldr f b

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\x -> Just (x, f x))

data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfoldb :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfoldb f a = case f a of
  Nothing -> Leaf
  Just (a, b, c) -> Node (unfoldb f a) b (unfoldb f c)


treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfoldb (\a -> if a <= n then Just (a+1, a, a+1) else Nothing) 0
