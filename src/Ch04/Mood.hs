module Ch04.Mood where

data Mood = Blah | Woot deriving (Show)

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _ = Woot

swap :: (b, a) -> (a, b)
swap (x, y) = (y, x)

tupFunc :: (Int, [a]) -> (Int, [a]) -> (Int, [a])
tupFunc (a, b) (c, d) = (a + c, b ++ d)
