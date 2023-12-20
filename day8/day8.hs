import Data.List (lookup)
import Data.Maybe (catMaybes)

data Node = Node Char Char Char deriving (Show, Eq)
data Direction = L | R deriving (Show, Eq)

explore :: [(Node, (Node, Node))] -> [Direction] -> Maybe Node -> Maybe [Direction]
explore _ _ Nothing = Nothing 
explore _ _ (Just (Node 'Z' 'Z' 'Z')) = Just []
explore xs (d:ds) (Just n)
    |target == Nothing = Nothing 
    |otherwise = let Just np = target 
                 in fmap (d:) $ explore xs ds $ Just $ take d np
    where 
        target :: Maybe (Node, Node)
        target = lookup n xs

        take :: Direction -> (a,a) -> a
        take L = fst
        take R = snd

explore' :: [(Node, (Node, Node))] -> [Direction] -> Maybe Node -> Maybe [Direction]
explore' _ _ Nothing = Nothing 
explore' _ _ (Just (Node _ _ 'Z')) = Just []
explore' xs (d:ds) (Just n)
    |target == Nothing = Nothing 
    |otherwise = let Just np = target 
                 in fmap (d:) $ explore' xs ds $ Just $ take d np
    where 
        target :: Maybe (Node, Node)
        target = lookup n xs

        take :: Direction -> (a,a) -> a
        take L = fst
        take R = snd

readCorresp :: String -> (Node, (Node, Node))
readCorresp ls = let (a:b:c:_:_:_:_:d:e:f:_:_:g:h:i:rest) = ls
                 in (Node a b c, (Node d e f, Node g h i))

readLR :: String -> [Direction]
readLR "" = []
readLR ('L':ls) = L:readLR ls
readLR ('R':ls) = R:readLR ls

format :: [String] -> ([Direction], [(Node, (Node, Node))])
format lss = let (xs:"":ys) =  lss
             in (readLR xs, map readCorresp ys)

run :: ([Direction], [(Node, (Node, Node))]) -> Maybe [Direction]
run (ds, cs) = explore cs (concat $ repeat ds) (Just (Node 'A' 'A' 'A'))

getInitialNodes :: [(Node, (Node, Node))] -> [Node]
getInitialNodes = filter (\(Node _ _ x) -> x == 'A') . map fst


run' :: ([Direction], [(Node, (Node, Node))]) -> [Maybe [Direction]]
run' (ds, cs) = map (explore' cs (concat $ repeat ds)) $ map Just $ getInitialNodes cs

maybeLcm :: Maybe Int -> Maybe Int -> Maybe Int
maybeLcm Nothing _ = Nothing 
maybeLcm _ Nothing = Nothing 
maybeLcm (Just m) (Just n) = Just $ lcm m n

main :: IO ()
main = do 
    contents <- readFile "input"
    putStr "Part 1: "
    print $ fmap length $ run $ format $ lines contents
    putStr "Part 2: "
    print $ foldl maybeLcm (Just 1) $ map (fmap length) $ run' $ format $ lines contents

