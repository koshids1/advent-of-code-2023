import Data.Char (isDigit, isSpace)
import Data.Maybe (mapMaybe)

data Color = Red | Green | Blue deriving (Show , Eq)
type Hand = (Color, Int)
type Game = (Int, [[Hand]])

splitBy :: Char -> String -> [String]
splitBy _ "" = []
splitBy x xs = (fst ysp) : splitBy x zs
    where
        ysp :: (String , String)
        ysp = break (== x) xs

        zs :: String
        zs = if (snd ysp == "") then "" else (tail $ snd ysp)
        
readHand :: String -> Maybe Hand 
readHand xs 
    |(fst ysp) /= "" && (snd ysp) == "red" = Just (Red , read $ fst ysp)
    |(fst ysp) /= "" && (snd ysp) == "green" = Just (Green , read $ fst ysp)
    |(fst ysp) /= "" && (snd ysp) == "blue" = Just (Blue , read $ fst ysp)
    |otherwise = Nothing
    where
        ysp :: (String , String)
        ysp = span (isDigit) xs

format :: String -> Game
format xs = (read $ filter (isDigit) $ fst ysp , map (mapMaybe readHand) wss)
    where
        ysp :: (String , String)
        ysp = break (== ':') $ filter (not . isSpace) xs

        zs :: [String]
        zs = splitBy ';' $ tail $ snd ysp

        wss :: [[String]]
        wss = map (splitBy ',') zs

isValidHand :: Hand -> Bool
isValidHand (Red , x)   = x <= 12
isValidHand (Green , x) = x <= 13
isValidHand (Blue , x)  = x <= 14

isValidGame :: Game -> Bool
isValidGame (_ , xss) = all (all isValidHand) xss

addUpInd :: [Game] -> Int
addUpInd []         = 0
addUpInd ((x,_):gs) = x + addUpInd gs

filterBy :: Color -> Game -> [Int]
filterBy color (_ , xss) = map snd $ concat $ map (filter (\ (c , _) -> (c == color))) xss

maxFor :: Color -> Game -> Int
maxFor color g 
    |xs /= []  = maximum xs 
    |otherwise = 0
    where
        xs :: [Int]
        xs = filterBy color g 

getPower :: Game -> Int
getPower g = (maxFor Red g) * (maxFor Green g) * (maxFor Blue g)

main :: IO ()
main = do
    contents <- readFile "input"
    putStr "Part 1: "
    print $ addUpInd $ filter isValidGame $ map format $ lines contents
    putStr "Part 2: "
    print $ sum $ map getPower $ map format $ lines contents
