module Day01 where

addIfNotBlank :: [String] -> String -> [String]
addIfNotBlank [] curr = [curr] 
addIfNotBlank (x:xs) curr
  | curr == "" = "0":x:xs
  | otherwise = show ((read curr :: Int) + (read x :: Int)):xs

keepBiggest :: Int -> String -> Int
keepBiggest prev curr
  | prev < (read curr :: Int) = read curr :: Int 
  | otherwise = prev

day01 :: IO ()
day01 = do
    content <- readFile "src/day01.input.txt"
    let contentLines = lines content
    let subTotals = foldl addIfNotBlank [] contentLines
    let maxValue = foldl keepBiggest 0 subTotals
    print maxValue

    return ()
