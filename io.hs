import Data.Char

main = do
  putStrLn "Hello"
  firstName <- getLine
  putStrLn "Last"
  lastName <- getLine
  putStrLn $ "Hey" ++ firstName ++ " is cool"
  let bigFirstName = map toUpper firstName
      bigLastName = map toUpper lastName
  putStrLn $ "Hey " ++ bigFirstName ++ " " ++ bigLastName
