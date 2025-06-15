import Data.Char
import System.IO

main = do
  contents <- readFile "girlfriend.txt"
  writeFile "girlfriendcaps.txt" (map toUpper contents)

-- main = do
--   withFile
--     "girlfriend.txt"
--     ReadMode
--     ( \handle -> do
--         contents <- hGetContents handle
--         putStr contents
--     )
