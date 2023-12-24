import Data.List(transpose, intersect)
import Data.Maybe(catMaybes)

data Cell = S | G deriving (Show, Eq)
data Coord = Coord Int Int deriving (Show, Eq)

zipWithCoords :: [[Cell]] -> [(Cell, Coord)]
zipWithCoords css = concat $ zipWith zip css [[Coord i j|j <- [1..]]|i <- [1..]]

getGalaxyCoords :: [(Cell, Coord)] -> [Coord]
getGalaxyCoords = map snd . filter (\(c,_) -> c == G)

getEmptyRows :: [[Cell]] -> [Int]
getEmptyRows = map fst . filter (\(_,cs) -> all (== S) cs) . zip [1..]

makeInterval :: Int -> Int -> [Int]
makeInterval x y 
    |x <= y = [x..y]
    |otherwise = [y..x]

sumOfPathLength :: Int -> [Int] -> [Int] -> [Coord] -> Int
sumOfPathLength _ _ _ [] = 0
sumOfPathLength expansion rows columns (c:cs) = go c cs + sumOfPathLength expansion rows columns cs
    where 
        go :: Coord -> [Coord] -> Int 
        go _ [] = 0 
        go c@(Coord x y) ((Coord z w):cs) = abs (z-x) + (expansion -1) * rowIntersection + abs (w-y) + (expansion -1) * columnIntersection + go c cs
            where 
                rowIntersection :: Int 
                rowIntersection = length $ rows `intersect` (makeInterval x z)

                columnIntersection :: Int 
                columnIntersection = length $ columns `intersect` (makeInterval y w)

run :: Int -> [[Cell]] -> Int 
run expansion css = sumOfPathLength expansion (getEmptyRows css) (getEmptyRows $ transpose css) $ getGalaxyCoords $ zipWithCoords css

format :: [String] -> [[Cell]]
format = map (catMaybes . readRow)
    where
        readRow :: String -> [Maybe Cell]
        readRow [] = []
        readRow ('.':cs) = (Just S):readRow cs 
        readRow ('#':cs) = (Just G):readRow cs
        readRow (_:cs) = Nothing:readRow cs

main :: IO ()
main = do 
    contents <- readFile "input"
    let cells = format $ lines contents
    putStr "Part 1: "
    print $ run 2 cells
    putStr "Part 2: "
    print $ run 1000000 cells
