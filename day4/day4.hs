import Data.List (intersect)

splitBy :: Char -> String -> [String]
splitBy _ "" = []
splitBy x xs = (fst ysp) : splitBy x zs
    where
        ysp :: (String , String)
        ysp = break (== x) xs

        zs :: String
        zs = if (snd ysp == "") then "" else (tail $ snd ysp)

dmap :: (a -> b) -> (a , a) -> (b , b)
dmap f (x,y) = (f x, f y)

format :: String -> ([Int] , [Int])
format xs = dmap (map read) $ dmap (filter (/= [])) $ dmap (splitBy ' ') (fst pairOfString, tail $ snd pairOfString)
    where
        withOutIndex :: String
        withOutIndex = tail $ dropWhile (/= ':') xs

        pairOfString :: (String , String)
        pairOfString = break (== '|') withOutIndex
        
calcPoint :: ([Int], [Int]) -> Int
calcPoint xs = if (num > 0) then 2 ^ (num - 1) else 0
    where
        ys :: [Int]
        ys = let (winning, inHand) = xs
             in winning `intersect` inHand
        
        num :: Int
        num = length ys

calcCards :: [([Int], [Int])] -> [Int]
calcCards = go (repeat 1)
    where
        go :: [Int] -> [([Int], [Int])] -> [Int]
        go [] _          = []
        go _ []          = []
        go (n:ns) (x:xs) = n:go (updateInit num n ns) xs
            where
                num :: Int
                num = length $ let (winning, inHand) = x in winning `intersect` inHand

                updateInit :: Int -> Int -> [Int] -> [Int]
                updateInit _ _ []     = []
                updateInit 0 _ ms     = ms
                updateInit k n (m:ms) = (m+n):updateInit (k-1) n ms
        
main :: IO ()
main = do 
    contents <- readFile "input"
    putStr "Part 1: "
    print $ sum $ map calcPoint $ map format $ lines contents
    putStr "Part 2: "
    print $ sum $ calcCards $ map format $ lines contents
