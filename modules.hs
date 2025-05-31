import Data.Char
import Data.List
import Data.Map qualified as Map

wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)

encode :: Int -> String -> String
encode offset = map (chr . (+ offset) . ord)

-- encode offset msg = map (\c -> chr $ ord c + offset) msg

decode :: Int -> String -> String
decode shift = encode (negate shift)

-- decode offset = map (chr . (+ negate offset) . ord)

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

firstToN :: Int -> Maybe Int
firstToN n = find (\x -> digitSum x == n) [1 ..]

phoneBook :: Map.Map String String
phoneBook =
  Map.fromList
    [ ("betty", "12345"),
      ("sam", "3847824")
    ]

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing

-- findKey key [] = Nothing
-- findKey key ((k,v):xs)
--     | key == k  = Just v
--     | otherwise = findKey key xs

string2digits :: String -> [Int]
string2digits = map digitToInt . filter isDigit
