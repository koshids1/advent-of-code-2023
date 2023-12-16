import Data.Char (isDigit)

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

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd = fmap

apply :: (Int, Int, Int) -> ([(Int,Int)], [(Int, Int)]) -> ([(Int,Int)], [(Int,Int)])
apply _ ([], xps) = ([], xps)
apply m@(t, i, r) ((j, s):rest, xps)
    |i+r <= j                       = mapFst ((j,s):) $ apply m (rest, xps)
    |i <= j && j < i+r && i+r < j+s = mapFst ((i+r, j+s-i-r):) $ mapSnd ((t+j-i,i+r-j):) $ apply m (rest, xps) 
    |i <= j && j+s <= i+r           = mapSnd ((t+j-i,s):) $ apply m (rest, xps)
    |j < i && i+r < j+s             = mapFst ((j,i-j):) $ mapFst ((i+r,j+s-i-r):) $ mapSnd ((t,r):) $ apply m (rest, xps)
    |j < i && i < j+s && j+s <= i+r = mapFst ((j, i-j):) $ mapSnd ((t,j+s-i):) $ apply m (rest, xps)
    |j+s <= i                       = mapFst ((j,s):) $ apply m (rest, xps)

send ::[(Int, Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
send ms xps = let (yps, zps) = go ms (xps, []) in yps ++ zps
    where
        go :: [(Int, Int, Int)] -> ([(Int,Int)], [(Int, Int)]) -> ([(Int, Int)], [(Int, Int)])
        go [] xpsp = xpsp 
        go (m:ms) xpsp = go ms $ apply m xpsp

sendComp :: [[(Int, Int, Int)]] -> [(Int, Int)] -> [(Int, Int)]
sendComp [] xps = xps 
sendComp (ms:mss) xps = sendComp mss $ send ms xps

format :: [String] -> ([Int],[[(Int, Int, Int)]])
format xss = (seeds, maps)
    where
        yss :: [[String]]
        yss = splitBy [] xss

        seeds :: [Int]
        seeds = map read $ splitBy ' ' $ dropWhile (not . isDigit) $ head (head yss)
        
        maps :: [[(Int, Int, Int)]]
        maps = map (map toTriple) $ map (map (map read)) $ map (map $ splitBy ' ') $ map tail $ tail yss

        toTriple :: [Int] -> (Int, Int, Int)
        toTriple xs = let k:l:m:rest = xs ++ [0,0..] in (k,l,m)

ammendSeeds1 :: [Int] -> [(Int, Int)]
ammendSeeds1 [] = []
ammendSeeds1 (x:xs) = (x,1):ammendSeeds1 xs

ammendSeeds2 :: [Int] -> [(Int,Int)]
ammendSeeds2 [] = []
ammendSeeds2 (x:[]) = []
ammendSeeds2 (x:y:rest) = (x,y):ammendSeeds2 rest

getLocation :: ([(Int, Int)], [[(Int, Int, Int)]]) -> [(Int, Int)]
getLocation x = (sendComp $ snd x) $ fst x

main :: IO ()
main = do
    contents <- readFile "input"
    putStr "Part 1: "
    print $ minimum $ map fst $ getLocation $ mapFst ammendSeeds1 $ format $ lines contents
    putStr "Part 2: "
    print $ minimum $ map fst $ getLocation $ mapFst ammendSeeds2 $ format $ lines contents
