doubleMe x = x + x
removeNonUppercase :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A' .. 'Z' ]]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- fst :: (a, b) -> a

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | weight / height ^ 2 <= 18.5 = "A"
  | weight / height ^ 2 <= 25.0 = "A"
  | weight / height ^ 2 < 30.0 = "A"
  | otherwise = "A"

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in  sideArea + 2 * topArea


initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname  

