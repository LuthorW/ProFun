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
  | x < 0     = error "Nao existe fatorial de numero negativo"
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

f1 :: [Char] -> [Char]
f1 [] = []
f1 [x] = [x]
f1 (x:y:xs)
  | isDigit x && not (isDigit y) = x : replicate (digitToInt x) y ++ f1 xs   -- Se 'x' for um número E 'y' NÃO for um número
  | otherwise                    = x : f1 (y:xs)   -- Para todos os outros casos (ex: letra seguida de letra, número seguido de número)
