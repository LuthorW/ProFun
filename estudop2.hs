
squaresum :: Int
squaresum = sum [x^2 | x <- [1..100]]

myreplicate :: Int -> a -> [a]
myreplicate x a = [a | _ <- [1..x]]

pyths :: Int -> [(Int, Int, Int)]
pyths x = [(a,b,c) | a <- [1..x], b <- [1..x], c <- [1..x], a^2+b^2 == c^2]

perfects :: Int -> [Int] 
perfects x = [a | a <- [1..x], sum(fatores a) == a]

fatores :: Int -> [Int] 
fatores x = [a | a <- [1..(x-1)], x `mod` a == 0]

conc :: [(Int, Int)]
conc = concat [[(x, y) | y <- [4,5,6]] | x <- [1,2,3]]

-- função que retorne a lista do cubo dos pares até o limite
evenCubs :: Int -> [Int]
evenCubs x = [n^3 | n <- [1..x], even n]

-- função que retorne a lista do cubo dos impares até o limite
odd4 :: Int -> [Int]
odd4 x = [n^4 | n <- [1..x], odd n]

-- função que insere na lista o parâmetro passado mantendo a lista ordenada
insertord :: Int -> [Int] -> [Int]
insertord a x = [y | y <- x, y <= a] ++ [a] ++ [y | y <- x, y> a]

-- função que retorna quantos múltiplos de um determinado inteiro tem em um intervalo
howmanymul :: Int -> Int -> Int -> Int
howmanymul x y z = length [a | a <- [y..z], a `mod` x == 0]

inverte :: [Int] -> [Int]
inverte x = foldl (\acc xs -> xs : acc) [] x

pushRight :: String -> Int -> String
pushRight x n = ['>' | _ <- [1..(n - length x)]] ++ x

conta :: [Char]-> Char -> Int
conta s c = length [x | x <- s, x == c]

proliferaInt :: [Int] -> [Int]
proliferaInt x = [a | a <- x, _ <- [1..a]]

proliferaChar :: String -> String
proliferaChar x = [a | a <- x, _ <- [1..length['A'..a]]]

converte :: [Int] -> String
converte x = [['A'..'Z'] !! (s-1) | s <- x]

