module Day05 where
import Data.Char (isDigit, isAlpha)
import Data.List (transpose)

toIndexCharTuples :: [Char] -> [(Int, Char)]
toIndexCharTuples = foldl (\acc curr -> acc ++ [(length acc, curr)]) []

getCharIndexes :: [String] -> [Int]
getCharIndexes xs = take (length xs + 2 // 4) [ 4 * x - 3 | x <- [1..]]

toStacks :: [Int] -> [(Int, Char)] -> [Char]
toStacks indexes xs = [ snd x | x <- xs, i <- indexes, fst x == i]

day05 :: IO ()
day05 = do
    content <- readFile "src/day05.example.txt"
    -- content <- readFile "src/day05.input.txt"
    let contentLines = lines content
    let stackLines = lines $ init $ takeWhile (not . isDigit) content
    print stackLines
    let stackTuplesPerLine = map toIndexCharTuples stackLines
    print stackTuplesPerLine
    -- wanted indexes are 1, 5, 9
    let possibleValues = take 3 [ 4 * x - 3 | x <- [1..]]
    print possibleValues
    let actualChars = map (toStacks possibleValues) stackTuplesPerLine 
    print actualChars
    let perStack = transpose actualChars
    print perStack
    let withoutSpaces = filter isAlpha <$> perStack
    print withoutSpaces

    return ()
