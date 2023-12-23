import Control.Monad (join)

data Direction = N | E | S | W deriving (Show, Eq)
data Pipe = V | H | NE | NW | SW | SE deriving (Show, Eq)
data Point = Point Int Int deriving (Show, Eq)
data State = State Point Direction

update :: [(Point, Maybe Pipe)] -> State -> State
update mss (State p d) = let Just (Just pipe) = lookup nextPoint mss 
                         in State nextPoint (updateDir pipe d)
    where
        nextPoint :: Point 
        nextPoint = updatePt d p
        
updatePt :: Direction -> Point -> Point 
updatePt N (Point x y) = Point (x-1) y
updatePt E (Point x y) = Point x (y+1) 
updatePt S (Point x y) = Point (x+1) y 
updatePt W (Point x y) = Point x (y-1)

updateDir :: Pipe -> Direction -> Direction 
updateDir V N = N
updateDir V S = S
updateDir H E = E
updateDir H W = W
updateDir NE S = E
updateDir NE W = N
updateDir NW S = W 
updateDir NW E = N 
updateDir SW N = W 
updateDir SW E = S 
updateDir SE N = E 
updateDir SE W = S

explore :: [(Point, Maybe Pipe)] -> State -> [Point]
explore mss s@(State p d) 
    |updatePt d p == (Point 0 0) = (Point 0 0):[]
    |otherwise = (updatePt d p):(explore mss (update mss s))

run :: [(Point, Maybe Pipe)] -> [Point]
run mss = explore mss (State (Point 0 0) (getInitialDirection mss))

format :: [String] -> [(Point, Maybe Pipe)]
format str = let (Point x y) = animalPt
             in concat $ zipWith zip [[Point i j|j<-[-y..]]|i<-[-x..]] $ map (map toPipe) str
    where 
        pairList :: [(Char, Point)]
        pairList = concat $ zipWith zip str $ [[Point i j|j<-[0..]]|i<-[0..]]

        animalPt :: Point 
        animalPt = let Just p = lookup 'S' pairList 
                 in p

toPipe :: Char -> Maybe Pipe 
toPipe '|' = Just V
toPipe '-' = Just H
toPipe 'L' = Just NE 
toPipe 'J' = Just NW
toPipe '7' = Just SW
toPipe 'F' = Just SE 
toPipe c = Nothing

getInitialDirection :: [(Point, Maybe Pipe)] -> Direction 
getInitialDirection mss = fst $ head dirList 
    where 
        dirList :: [(Direction, Maybe Pipe)]
        dirList = filter (`elem` validPairs) $ map (fmap join) [(N, lookup (Point (-1) 0) mss), (E, lookup (Point 0 1) mss), (S, lookup (Point 1 0) mss), (W, lookup (Point 0 (-1)) mss)]

        validPairs :: [(Direction, Maybe Pipe)]
        validPairs = [(N, Just V), (N, Just SW), (N, Just SW),
                      (E, Just H), (E, Just NW), (E, Just SW),
                      (S, Just V), (S, Just NE), (S, Just NW),
                      (W, Just H), (W, Just NE), (W, Just SE)]

getArea :: [Point] -> Int
getArea ps = (go ((Point 0 0):ps) - length ps + 2) `div` 2
    where 
        go :: [Point] -> Int 
        go [] = 0
        go (p:[]) = 0
        go ((Point x y):(Point z w):ps) = (w + y) * (z - x) + go ((Point z w):ps)


main :: IO ()
main = do 
    contents <- readFile "input"
    let loop = run $ format $ lines $ contents
    putStr "Part 1: "
    print $ (length loop) `div` 2
    putStr "Part 2: "
    print $ getArea loop
