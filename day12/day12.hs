
getNumber :: String -> [Int] -> Int
getNumber [] [] = 1
getNumber [] (n:[]) = 0
getNumber s []
    |all (/= '#') s = 1
    |otherwise = 0
getNumber ('.':rest) ns = getNumber rest ns
getNumber ('?':rest) ns = getNumber rest ns + getNumber ('#':rest) ns
getNumber s (n:ns)
    |length s >= n && checkInterior && checkEnd = getNumber (drop (n+1) s) ns
    |otherwise = 0
    where 
        checkInterior :: Bool 
        checkInterior = all (/='.') $ take n s

        checkEnd :: Bool
        checkEnd = all (/='#') $ take 1 $ drop n s

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

format :: [String] -> [(String, [Int])]
format = map go
    where
        go :: String -> (String, [Int])
        go = fmap (map read) . fmap (splitBy ',') . break (==' ')

makeNewDataSet :: [(String, [Int])] -> [(String, [Int])]
makeNewDataSet = map go
    where
        go :: (String, [Int]) -> (String, [Int])
        go (s, ns) = (drop 1 $ concat $ take 5 $ repeat ('?':s), concat $ take 5 $ repeat ns)

run :: [(String, [Int])] -> Int 
run [] = 0
run ((s,ns):rest) = (getNumber s ns) + (run rest)

main :: IO ()
main = do 
    contents <- readFile "example"
    let parsed = format $ lines contents
    print $ run parsed
    let newParsed = makeNewDataSet parsed 
--    print $ run $ newParsed -- This doesn't finish computation.
