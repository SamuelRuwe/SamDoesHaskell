import Control.Exception
import Data.List
import System.Directory
import System.Environment
import System.IO

dispatch :: String -> [String] -> IO ()
dispatch "add" = add
dispatch "bump" = bump
dispatch "view" = view
dispatch "remove" = remove
dispatch command = doesntExist command

main = do
  (command : argList) <- getArgs
  dispatch command argList

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
add _ = putStrLn "The add command takes exactly two arguments"

view :: [String] -> IO ()
view [fileName] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0 ..] todoTasks
  putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, numberString] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0 ..] todoTasks
  putStrLn "These are your items"
  mapM_ putStrLn numberedTasks
  let number = read numberString
      newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
  bracketOnError
    (openTempFile "." "temp")
    ( \(tempName, tempHandle) -> do
        hClose tempHandle
        removeFile tempName
    )
    ( \(tempName, tempHandle) -> do
        hPutStr tempHandle newTodoItems
        hClose tempHandle
        removeFile fileName
        renameFile tempName fileName
    )

bump :: [String] -> IO ()
bump [fileName, numberString] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      number = read numberString
      head = todoTasks !! number
      tail = delete (todoTasks !! number) todoTasks
      newTodoItems = unlines $ head : tail
  bracketOnError
    (openTempFile "." "temp")
    ( \(tempName, tempHandle) -> do
        hClose tempHandle
        removeFile tempName
    )
    ( \(tempName, tempHandle) -> do
        hPutStr tempHandle newTodoItems
        hClose tempHandle
        removeFile fileName
        renameFile tempName fileName
    )

doesntExist :: String -> [String] -> IO ()
doesntExist command _ =
  putStrLn $ "The " ++ command ++ " command does not exist"
