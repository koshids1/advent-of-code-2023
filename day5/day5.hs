import Data.Char (isDigit)

data Interval = Interval {initial :: Int, range :: Int}
data MapTriple = MapTriple Int Int Int

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

apply :: MapTriple -> ([Interval], [Interval]) -> ([Interval], [Interval])
apply _ ([], xs) = ([], xs)
apply mt@(MapTriple t i r) ((Interval j s):ys, xs)
    |i+r <= j                       = mapFst (Interval j s:) $ apply mt (ys, xs)
    |i <= j && j < i+r && i+r < j+s = mapFst (Interval (i+r) (j+s-i-r):) $ mapSnd (Interval (t+j-i) (i+r-j):) $ apply mt (ys, xs) 
    |i <= j && j+s <= i+r           = mapSnd (Interval (t+j-i) s:) $ apply mt (ys, xs)
    |j < i && i+r < j+s             = mapFst (Interval j (i-j):) $ mapFst (Interval (i+r) (j+s-i-r):) $ mapSnd (Interval t r:) $ apply mt (ys, xs)
    |j < i && i < j+s && j+s <= i+r = mapFst (Interval j (i-j):) $ mapSnd (Interval t (j+s-i):) $ apply mt (ys, xs)
    |j+s <= i                       = mapFst ((Interval j s):) $ apply mt (ys, xs)
    

send :: [MapTriple] -> [Interval] -> [Interval]
send mts xs = let (ys, zs) = go mts (xs, []) 
               in ys ++ zs
    where
        go :: [MapTriple] -> ([Interval], [Interval]) -> ([Interval], [Interval])
        go [] xsp = xsp 
        go (mt:mts) xsp = go mts $ apply mt xsp

sendComp :: [[MapTriple]] -> [Interval] -> [Interval]
sendComp [] xs = xs 
sendComp (mts:mtss) xs = sendComp mtss $ send mts xs

format :: [String] -> ([Int],[[MapTriple]])
format xss = (seeds, maps)
    where
        yss :: [[String]]
        yss = splitBy [] xss

        seeds :: [Int]
        seeds = map read $ splitBy ' ' $ dropWhile (not . isDigit) $ head (head yss)
        
        maps :: [[MapTriple]]
        maps = map (map toTriple) $ map (map (map read)) $ map (map $ splitBy ' ') $ map tail $ tail yss

        toTriple :: [Int] -> MapTriple
        toTriple xs = let k:l:m:rest = xs ++ [0,0..] in MapTriple k l m

ammendSeeds1 :: [Int] -> [Interval]
ammendSeeds1 [] = []
ammendSeeds1 (x:xs) = (Interval x 1):ammendSeeds1 xs

ammendSeeds2 :: [Int] -> [Interval]
ammendSeeds2 [] = []
ammendSeeds2 (x:[]) = []
ammendSeeds2 (x:y:xs) = (Interval x y):ammendSeeds2 xs

getLocation :: ([Interval], [[MapTriple]]) -> [Interval]
getLocation x = (sendComp $ snd x) $ fst x

main :: IO ()
main = do
    contents <- readFile "input"
    putStr "Part 1: "
    print $ minimum $ map initial $ getLocation $ mapFst ammendSeeds1 $ format $ lines contents
    putStr "Part 2: "
    print $ minimum $ map initial $ getLocation $ mapFst ammendSeeds2 $ format $ lines contents
