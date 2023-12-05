import Data.Char (isDigit)

translate :: String -> String
translate [] = []
translate ('o':'n':'e':xs) = '1':translate ('n':'e':xs)
translate ('t':'w':'o':xs) = '2':translate ('w':'o':xs)
translate ('t':'h':'r':'e':'e':xs) = '3':translate ('h':'r':'e':'e':xs)
translate ('f':'o':'u':'r':xs) = '4':translate ('o':'u':'r':xs)
translate ('f':'i':'v':'e':xs) = '5':translate ('i':'v':'e':xs) 
translate ('s':'i':'x':xs) = '6':translate ('i':'x':xs) 
translate ('s':'e':'v':'e':'n':xs) = '7':translate ('e':'v':'e':'n':xs)  
translate ('e':'i':'g':'h':'t':xs) = '8':translate ('i':'g':'h':'t':xs) 
translate ('n':'i':'n':'e':xs) = '9':translate ('i':'n':'e':xs) 
translate (x:xs) = x:(translate xs)

extractNum :: String -> Int
extractNum xs = read $ (head ys) : (last ys) : []
    where
        ys :: String
        ys = filter isDigit xs

extractNum' :: String -> Int
extractNum' xs = read $ (head ys) : (last ys) : []
    where
        ys :: String
        ys = (filter isDigit) $ translate xs

addUp :: [String] -> Int
addUp = sum . (map extractNum)

addUp' :: [String] -> Int
addUp' = sum . (map extractNum')

main :: IO ()
main = do
    contents <- readFile "input"
    putStr "Part 1: "
    print $ addUp $ lines contents
    putStr "Part 2: "
    print $ addUp' $ lines contents
