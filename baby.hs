doubleMe x = x + x

triples = [ (a, b, c) | c <- [1..10], a <- [1..10], b <- [1..10]]

lucky :: Int -> String
lucky 7 = "WIn!"
lucky x = "Lose!"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (a, _, _) = a

tell :: (Show a) => [a] -> String
tell []  = "EMPTY"
tell (x:[]) = "One element" ++ show x
tell (x:y:[]) = "Two element" ++ show x ++ "and" ++ show y
tell (x:y:_) = "Long List"

firstLetter :: String -> String
firstLetter "" = "Empty string!"
firstLetter all@(x:xs) = "The first letter is " ++ [x]

myCompare :: (Ord a) => a -> a -> Ordering
-- a `myCompare` b
--     | a == b =    EQ
--     | a <= b =    LT
--     | otherwise = GT

myCompare a b
    | a == b =    EQ
    | a <= b =    LT
    | otherwise = GT

bmiTell :: Double -> Double -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, eat more!"
    | bmi <= normal = "Looking good!"
    | bmi <= fat    = "You're overweight. Let's work out together!"
    | otherwise     = "You're obese. Go see a doctor"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat = 30.0

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2
