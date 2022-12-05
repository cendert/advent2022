module Day03 where
import Text.Printf (printf)

splitToEvenParts :: [a] -> ([a], [a])
splitToEvenParts xs = splitAt (length xs `div` 2) xs

toThruple :: [String] -> [(String, String, String)]
toThruple [] = []
toThruple (x1:x2:x3:xs) = (x1,x2,x3) : toThruple xs

findDuplicate :: Eq a => ([a], [a]) -> a
findDuplicate (xs, ys) = head [x | x <- xs, y <- ys, x == y]

findCommonValueInThruple :: Eq a => ([a], [a], [a]) -> a
findCommonValueInThruple (xs, ys, zs) = head [ x | x <-xs, y <- ys, x == y, z <- zs, x == z]

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
    let prioValues = map (getCharIndex charIndexMapping) duplicates
    let prioTotal = sum prioValues
    printf "The total prio based on the duplicate values within the rucksacks is %i\n" prioTotal

    let perGroupOfThree = toThruple contentLines
    let commonValues = map findCommonValueInThruple perGroupOfThree
    let prioValuesOfGroupBadges = map (getCharIndex charIndexMapping) commonValues
    let prioTotalOfGroupBadges = sum prioValuesOfGroupBadges
    printf "The total prio based on the common values within three rucksacks is %i\n" prioTotalOfGroupBadges
    
    return ()
