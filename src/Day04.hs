module Day04 where
import Data.Char (isDigit)

toDigitStringList :: String -> [String]
toDigitStringList x 
  | dropWhile isDigit x == "" = [x]
  | otherwise = takeWhile isDigit x : toDigitStringList (tail (dropWhile isDigit x))

toIntTuple :: [String] -> (Int, Int, Int, Int)
toIntTuple (f:g:h:i:_) = ( read f :: Int, read g :: Int, read h :: Int, read i :: Int)

containsTheOther :: (Ord a1, Ord a2, Num a3) => (a1, a2, a1, a2) -> a3
containsTheOther (x1,x2,y1,y2)
  | x1 <= y1 && x2 >= y2 = 1
  | x1 >= y1 && x2 <= y2 = 1
  | otherwise = 0

day04 :: IO ()
day04 = do
    -- content <- readFile "src/day04.example.txt"
    content <- readFile "src/day04.input.txt"
    let contentLines = lines content
    let contentStringLists = map toDigitStringList contentLines
    let contentIntTuples = map toIntTuple contentStringLists
    let superSets = map containsTheOther contentIntTuples
    let countedSuperSets = sum superSets
    print countedSuperSets

    return ()
