import Data.Char (isDigit)

data Time = Time Int deriving Show
data Distance = Distance Int deriving Show

getDistance :: Time -> Time -> Distance 
getDistance (Time t) (Time s) = Distance $ s * (t -s)

getNumOptions :: Time -> Distance -> Int 
getNumOptions (Time t) (Distance d) = t + 1- 2 * (length $ takeWhile (\(Distance k) -> k <= d) $ map (getDistance (Time t)) $ map Time [0..])

getTotNumOptions :: [(Time, Distance)] -> Int 
getTotNumOptions = product . map (\(t, d) -> getNumOptions t d)

format :: [String] -> [(Time, Distance)]
format (xs:ys:rest) = zip (map Time $ readString xs) (map Distance $ readString ys)
    where 
        readString :: String -> [Int]
        readString = (map read) . (filter (/="")) . tail . (splitBy ' ')

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

format' :: [String] -> (Time, Distance)
format' xss = let (t:d:rest) = map read $ map (filter (isDigit)) xss
              in (Time t, Distance d)

main :: IO ()
main = do 
    contents <- readFile "input"
    putStr "Part 1: "
    print $ getTotNumOptions $ format $ lines contents
    putStr "Part 2: "
    print $ (\(t, d) -> getNumOptions t d) $ format' $ lines contents
