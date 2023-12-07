import Data.Char (isDigit)

data Coords = Coords {x :: Int , y :: Int} deriving (Show, Eq)

makeStrip :: Coords -> [Coords]
makeStrip p = [north p, p , south p]
    where
        north :: Coords -> Coords
        north (Coords x y) = Coords x (y - 1)

        south :: Coords -> Coords
        south (Coords x y) = Coords x (y + 1)

zipWithCoords :: [String] -> [[(Char, Coords)]]
zipWithCoords lss = zipWith zip lss coords
    where
        coords :: [[Coords]]
        coords = [[(Coords i j)|i<-[1..]]|j<-[1..]]

getCoordsOfSymbols :: [[(Char, Coords)]] -> [Coords]
getCoordsOfSymbols = concat . map (map snd) . map (filter (isSymbol' . fst))
    where
        isSymbol' :: Char -> Bool
        isSymbol' c = not $ isDigit c || c == '.'

getCoordsOfStars :: [[(Char , Coords)]] -> [Coords]
getCoordsOfStars = concat . map (map snd) . map (filter (isStar . fst))
    where
        isStar :: Char -> Bool
        isStar c = c == '*'

getDigitsWithNeighbors :: [(Char, Coords)] -> [(String , [Coords])]
getDigitsWithNeighbors [] = []
getDigitsWithNeighbors ((c,p):[])
    |isDigit c = [([c], makeStrip p)]
    |otherwise = []
getDigitsWithNeighbors ((c,p):(d,q):xs)
    |isDigit c && isDigit d =  addCoordsStrip p $ addCharHead c $ getDigitsWithNeighbors ((d,q):xs)
    |isDigit c && (not . isDigit) d = addCoordsStrip p $ addCoordsStrip q $ addCharNew c $ getDigitsWithNeighbors ((d,q):xs)
    |(not. isDigit) c && isDigit d = addCoordsStrip p $ getDigitsWithNeighbors ((d,q):xs)
    |otherwise = getDigitsWithNeighbors ((d,q):xs)
    where
        addCharHead :: Char -> [(String , [Coords])] -> [(String, [Coords])]
        addCharHead c [] = [([c],[])]
        addCharHead c ((ls, ps):xs) = (c:ls , ps):xs

        addCharNew :: Char -> [(String , [Coords])] -> [(String, [Coords])]
        addCharNew c xs = ([c],[]):xs

        addCoordsStrip :: Coords -> [(String, [Coords])] -> [(String, [Coords])]
        addCoordsStrip p [] = [("", makeStrip p)]
        addCoordsStrip p ((ls , ps):xs) = (ls,(makeStrip p) ++ ps):xs

getNumbersWithNeighbors :: [[(Char, Coords)]] -> [(Int, [Coords])]
getNumbersWithNeighbors = map (\(xs,ys) -> (read xs,ys)) . concat . map getDigitsWithNeighbors

isAdjacentTo :: Coords -> (Int, [Coords]) -> Bool
isAdjacentTo p (_,qs) = p `elem` qs

getPartNumbers :: [String] -> [Int]
getPartNumbers xss = map fst $ filter isPartNumber numbersWithNeighbors
    where
        yss :: [[(Char , Coords)]]
        yss = zipWithCoords xss

        coordsOfSymbols :: [Coords]
        coordsOfSymbols = getCoordsOfSymbols yss

        numbersWithNeighbors :: [(Int, [Coords])]
        numbersWithNeighbors = getNumbersWithNeighbors yss

        isPartNumber :: (Int, [Coords]) -> Bool
        isPartNumber x = any (\p -> isAdjacentTo p x) coordsOfSymbols

getGearRatios :: [String] -> [Int]
getGearRatios xss = map product $ map (map fst) $ filter (\zs -> (length zs == 2)) $ map (\p -> filter (isAdjacentTo p) numbersWithNeighbors) coordsOfStars 
    where
        yss :: [[(Char, Coords)]]
        yss = zipWithCoords xss

        coordsOfStars :: [Coords]
        coordsOfStars = getCoordsOfStars yss

        numbersWithNeighbors :: [(Int, [Coords])]
        numbersWithNeighbors = getNumbersWithNeighbors yss

main :: IO ()
main = do
    contents <- readFile "input"
    putStr "Part 1: "
    print $ sum $ getPartNumbers $ lines contents
    putStr "Part 2: "
    print $ sum $ getGearRatios $ lines contents
