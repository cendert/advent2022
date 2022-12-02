module Day01 where

sumTillNextSpaceChar :: [String] -> String -> [String]
sumTillNextSpaceChar [] curr = [curr] 
sumTillNextSpaceChar (x:xs) curr
  | curr == "" = "0":x:xs
  | otherwise = show ((read curr :: Int) + (read x :: Int)):xs

keepBiggest :: Int -> String -> Int
keepBiggest acc curr
  | acc < (read curr :: Int) = read curr :: Int 
  | otherwise = acc

keepTopThree :: [Int] -> Int -> [Int]
keepTopThree [] curr = [curr]
keepTopThree [x] curr
  | x < curr = curr:[x]
  | otherwise = x:[curr]
keepTopThree [x1, x2] curr
  | x1 < curr = curr:x1:[x2]
  | x2 < curr = x1:curr:[x2]
  | otherwise = x1:x2:[curr]
keepTopThree (x1:x2:x3:_) curr
  | x1 < curr = curr:x1:[x2]
  | x2 < curr = x1:curr:[x2]
  | x3 < curr = x1:x2:[curr]
  | otherwise = x1:x2:[x3]

day01 :: IO ()
day01 = do
    content <- readFile "src/day01.input.txt"
    let contentLines = lines content
    let rationsPerElf = foldl sumTillNextSpaceChar [] contentLines
    let maxValue = foldl keepBiggest 0 rationsPerElf
    print maxValue
    let sumOfTopThreeElves = sum $ foldl keepTopThree [] $ map read rationsPerElf
    print sumOfTopThreeElves

    return ()
