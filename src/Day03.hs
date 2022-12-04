module Day03 where

splitToEvenParts :: [a] -> ([a], [a])
splitToEvenParts xs = splitAt (length xs `div` 2) xs

findDuplicate :: Eq a => ([a], [a]) -> a
findDuplicate (xs, ys) = head [x | x <- xs, y <- ys, x == y]

lowerCaseIndexes :: [(Char, Integer)]
lowerCaseIndexes = zip ['a'..'z'] [1..26]

upperCaseIndexes :: [(Char, Integer)]
upperCaseIndexes = zip ['A'..'Z'] [27..52]

charIndexMapping :: [(Char, Integer)]
charIndexMapping = lowerCaseIndexes ++ upperCaseIndexes

getCharIndex :: Eq a1 => [(a1, a2)] -> a1 -> a2
getCharIndex m ch = head [i | (c,i) <- m, ch == c]



day03 :: IO ()
day03 = do
    -- content <- readFile "src/day03.example.txt"
    content <- readFile "src/day03.input.txt"
    let contentLines = lines content
    let evenParts = map splitToEvenParts contentLines
    let duplicates = map findDuplicate evenParts
    -- print duplicates
    let prioValues = map (getCharIndex charIndexMapping) duplicates
    let prioTotal = sum prioValues
    -- print prioValues
    print prioTotal
    
    return ()
