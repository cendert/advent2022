module Day05 where
import Data.Char (isDigit, isAlpha, digitToInt)
import Data.List (transpose)

toIndexCharTuples :: [Char] -> [(Int, Char)]
toIndexCharTuples = foldl (\acc curr -> acc ++ [(length acc, curr)]) []

getCharIndexes :: [[(Int,Char)]] -> [Int]
getCharIndexes xs = take (length xs + 2 `div` 4) [ 4 * x - 3 | x <- [1..]]

toStacks :: [Int] -> [(Int, Char)] -> [Char]
toStacks indexes xs = [ snd x | x <- xs, i <- indexes, fst x == i]

data Instruction = Instruction {
    amount :: Int,
    source :: Int,
    destination :: Int
} deriving (Read, Show)

newtype State = State [[Char]]
newtype Index = Index Int

toInstruction :: String -> Maybe Instruction
toInstruction xs = do
    let digitCharList = filter isDigit xs
    let digitList = map ((\x -> x -1) . digitToInt) digitCharList
    let result = Instruction { amount = head digitList + 1, source = digitList !! 1, destination = digitList !! 2}
    return result

applyInstructions :: [[Char]] -> Maybe Instruction -> [[Char]]
applyInstructions acc Nothing = acc
applyInstructions acc (Just curr) = acc
-- applyInstructions acc (Just curr) = do
--     let newSource = drop (amount curr) (acc !! source curr)
--     let newDestination = reverse $ take (amount curr) (acc !! source curr ) ++ acc !! destination curr
--     return ['a','b']

-- updateState :: Instruction -> Index -> State -> State
-- updateState :: Instruction -> Int -> [[a]] -> [[a]]
updateState :: Int -> [[Char]] -> Maybe Instruction -> [[Char]]
updateState _ before Nothing = before
updateState index before (Just instr)
  | source instr == index = drop (amount instr) (before !! index) : updateState (index + 1) before (Just instr)
  | destination instr == index = (reverse (take (amount instr) (before !! source instr)) ++ before !! index) : updateState (index + 1) before (Just instr)
  | index == length before = []
  | otherwise = before !! index : updateState (index + 1) before (Just instr)


day05 :: IO ()
day05 = do
    content <- readFile "src/day05.example.txt"
    -- content <- readFile "src/day05.input.txt"
    let contentLines = lines content
    let stackLines = lines $ init $ takeWhile (not . isDigit) content
    let stackTuplesPerLine = map toIndexCharTuples stackLines
    let charIndexes = getCharIndexes stackTuplesPerLine
    let actualChars = map (toStacks charIndexes) stackTuplesPerLine
    let perStack = transpose actualChars
    let initialState = filter isAlpha <$> perStack
    print initialState
    
    let instructionStrings = lines $ dropWhile ('m' /=) content
    let parsedInstructions = map toInstruction instructionStrings
    print parsedInstructions
    -- let test = map (updateState 0 initialState) parsedInstructions
    -- print test
    -- let single = updateState 0 initialState $ head parsedInstructions
    -- print single
    let test2 = foldl (updateState 0) initialState parsedInstructions
    print test2
    -- let test3 = foldr (updateState 0) initialState (reverse parsedInstructions)
    -- let results = foldl applyInstructions withoutSpaces parsedInstructions
    -- print results

    return ()
