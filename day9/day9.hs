getDiff :: [Int] -> [Int]
getDiff [] = []
getDiff (x:[]) = []
getDiff (x:y:xs) = (y-x):getDiff (y:xs)

getTable :: [Int] -> [[Int]]
getTable [] = []
getTable ns
    |all (==0) ns = [ns]
    |otherwise = ns:(getTable $ getDiff ns)

getNext :: [Int] -> Int 
getNext = sum . map last . getTable

getPrev :: [Int] -> Int
getPrev = foldr (-) 0 . map head . getTable

sumOfExtrapolatedValues :: [[Int]] -> Int
sumOfExtrapolatedValues = sum . map getNext

sumOfBackwardExtrapolation :: [[Int]] -> Int 
sumOfBackwardExtrapolation = sum . map getPrev

format :: [String] -> [[Int]]
format = map readSeq
    where
        readSeq :: String -> [Int]
        readSeq = map read . splitBy ' '

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy _ [] = []
splitBy x xs = (fst $ breakBy x xs) : splitBy x (snd $ breakBy x xs)
    where
        breakBy :: Eq a => a -> [a] -> ([a], [a])
        breakBy _ [] = ([], [])
        breakBy x (y:ys)
            |x == y = ([], ys)
            |otherwise = mapFst (y:) $ breakBy x ys

        mapFst :: (a -> c) -> (a, b) -> (c, b)
        mapFst f (x, y) = (f x, y)

main :: IO ()
main = do
    contents <- readFile "input"
    putStr "Part 1: "
    print $ sumOfExtrapolatedValues $ format $ lines contents
    putStr "Part 2: "
    print $ sumOfBackwardExtrapolation $ format $ lines contents
