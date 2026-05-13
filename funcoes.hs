import Data.Char

f2 :: Float -> Float -> Float
f2 x y
  | x >= y    = x + y
  | otherwise = x - y

f3 :: Float -> Float -> Float -> Float
f3 x y z
  | x + y > z = x + y + z
  | x + y < z = x - y - z
  | otherwise = 0

fat :: Int -> Int
fat x
  | x < 0     = error "Nao existe"
  | x == 0    = 1
  | otherwise = x * fat (x - 1)

{- 01 função que soma os elementos de uma lista -}
sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

{- 02-localiza elemento em lista -}
searchList::Int->[Int]->Bool
searchList _ [] = False
searchList n (x:xs)
  | n == x   = True
  | otherwise = searchList n xs

{-03 remove todas ocorrências de y em uma lista -}
deleteList::Int->[Int]->[Int]
deleteList _ [] = []
deleteList y (x:xs)
  | y == x   = deleteList y xs
  | otherwise = x : deleteList y xs

reverte :: [a] -> [a]
reverte   []  = []
reverte (b:x) = reverte x ++ [b]

f1 :: [Char] -> [Char]
f1 [] = []
f1 [x] = [x]
f1 (x:y:xs)
  | isDigit x && not (isDigit y) = x : replicate (digitToInt x) y ++ f1 xs   -- se x for um número e y nao
  | otherwise                    = x : f1 (y:xs)   


f11 :: String -> [(Char, Bool, Int)]
f11 [] = []
f11 [x] = [(x, False, 1)]
f11 (x:y:xs)
  | isDigit x && not (isDigit y) = (x, False, 1) : (y, True, digitToInt x) : f11 xs
  | otherwise                    = (x, False, 1) : f11 (y:xs)

f22 :: [String] -> [String]
f22 [] = []
f22 (x:xs) = f1 x : f22 xs

lastDigit :: Int -> Int
lastDigit n = mod n 10

square :: Int -> Int
square x = x * x

fourPower :: Int -> Int
fourPower x = square (square x)
