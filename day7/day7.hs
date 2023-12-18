import Data.List (sort, sortBy, group)
import Data.Maybe (catMaybes)

data Hand = A | K | Q | J | T | Nine | Eight | Seven | Six | Five | Four | Three | Two deriving (Show, Eq, Ord)

data Type = FiveOfAKind | FourOfAKind | FullHouse | ThreeOfAKind | TwoPair | OnePair | HighCard deriving (Show, Eq, Ord)

data Bid = Bid Int deriving Show 
data Rank = Rank Int deriving Show

toHand :: Char -> Maybe Hand 
toHand 'A' = Just A 
toHand 'K' = Just K 
toHand 'Q' = Just Q 
toHand 'J' = Just J 
toHand 'T' = Just T 
toHand '9' = Just Nine
toHand '8' = Just Eight
toHand '7' = Just Seven
toHand '6' = Just Six
toHand '5' = Just Five
toHand '4' = Just Four
toHand '3' = Just Three
toHand '2' = Just Two
toHand _ = Nothing

readHands :: String -> [Hand] 
readHands = catMaybes . (map toHand)

sortByLength :: [[a]] -> [[a]]
sortByLength xs = sortBy compare xs
    where
        compare :: [a] -> [a] -> Ordering
        compare xs ys
            |length xs < length ys = GT
            |length xs == length ys = EQ 
            |length xs > length ys = LT

intsToType :: [Int] -> Type
intsToType (5:_)   = FiveOfAKind 
intsToType (4:_)   = FourOfAKind 
intsToType (3:2:_) = FullHouse 
intsToType (3:_)   = ThreeOfAKind 
intsToType (2:2:_) = TwoPair 
intsToType (2:_)   = OnePair 
intsToType _       = HighCard

getType :: [Hand] -> Type 
getType = intsToType . map length . sortByLength . group . sort

getType' :: [Hand] -> Type
getType' xs = let (ys, zs) = break (== J) $ sortBy newOrderHand xs
              in intsToType $ map length $ addToHead zs $ sortByLength $ group ys 
    where
        addToHead :: [a] -> [[a]] -> [[a]]
        addToHead xs [] = [xs]
        addToHead xs (ys:yss) = (xs++ys):yss

sortByStrength :: [([Hand], Bid)] -> [(Rank, [Hand], Bid)]
sortByStrength = reverse. zip' (map Rank [1..]) . reverse. map dropFst . sortBy (\(t,h,_) (s,k,_) -> compare (t,h) (s,k)) . map (\(h, b) -> (getType h, h, b))
    where 
        dropFst :: (a,b,c) -> (b,c)
        dropFst (_,y,z) = (y,z)

        zip' :: [a] -> [(b,c)] -> [(a,b,c)]
        zip' [] _ = []
        zip' _ [] = []
        zip' (x:xs) ((y,z):yzs) = (x,y,z):zip' xs yzs

        compare :: (Type, [Hand]) -> (Type, [Hand]) -> Ordering 
        compare (t,h) (s, k)
            |t < s = LT 
            |t == s && h < k = LT 
            |t == s && h == k = EQ
            |t == s && h > k = GT 
            |t > s = GT 

sortByStrength' :: [([Hand], Bid)] -> [(Rank, [Hand], Bid)]
sortByStrength' = reverse. zip' (map Rank [1..]) . reverse. map dropFst . sortBy (\(t,h,_) (s,k,_) -> compare (t,h) (s,k)) . map (\(h, b) -> (getType' h, h, b))
    where 
        dropFst :: (a,b,c) -> (b,c)
        dropFst (_,y,z) = (y,z)

        zip' :: [a] -> [(b,c)] -> [(a,b,c)]
        zip' [] _ = []
        zip' _ [] = []
        zip' (x:xs) ((y,z):yzs) = (x,y,z):zip' xs yzs

        compare :: (Type, [Hand]) -> (Type, [Hand]) -> Ordering 
        compare (t,h) (s, k)
            |t < s = LT 
            |t == s = newLexicoHands h k
            |t > s = GT 

totalWinning :: [(Rank, [Hand], Bid)] -> Int 
totalWinning = foldl calc 0
    where
        calc :: Int -> (Rank, [Hand], Bid) -> Int 
        calc x (Rank r,_ , Bid b) = x + r * b

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

format :: [String] -> [([Hand], Bid)]
format = map formatLine
    where 
        formatLine :: String -> ([Hand], Bid)
        formatLine xs = let (ys:zs:_) = filter (/="") $ splitBy ' ' xs
                        in (readHands ys, Bid $ read zs)

newOrderHand :: Hand -> Hand -> Ordering 
newOrderHand J J = EQ
newOrderHand J _ = GT 
newOrderHand _ J = LT 
newOrderHand h k = compare h k

newLexicoHands :: [Hand] -> [Hand] -> Ordering 
newLexicoHands [] [] = EQ
newLexicoHands [] _ = LT 
newLexicoHands _ [] = GT 
newLexicoHands (h:hs) (k:ks) 
    |newOrderHand h k == LT = LT 
    |newOrderHand h k == GT = GT 
    |newOrderHand h k == EQ = newLexicoHands hs ks

main :: IO ()
main = do 
    contents <- readFile "input"
    putStr "Part 1: "
    print $ totalWinning $ sortByStrength $ format $ lines contents
    putStr "Part 2: "
    print $ totalWinning $ sortByStrength' $ format $ lines contents
