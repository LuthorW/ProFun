separa :: [Int] -> ([Int], [Int])
separa x = ([a | a <- x, odd a], [a | a <- x, even a])

inverte :: [Int] -> [Int]
inverte x = foldl (\acc n -> n : acc) [] x

prolifera :: [Int] -> [Int]
prolifera x = [a | a <- x, _ <- [1..a]]

evencub :: Int -> [Int]
evencub x = [a^3 | a <- [1..x], even a]

sqrsum :: Int
sqrsum = sum [x^2 | x <- [1..100]]

replicatem :: Int -> a -> [a]
replicatem x a = [a | _ <- [1..x]]

pyths :: Int -> [(Int, Int, Int)]
pyths x = [(a,b,c) | a <- [1..x], b <- [1..x], c <- [1..x], a^2 + b^2 == c^2]

dec2int :: [Int] -> Int
dec2int = foldl (\acc a -> acc * 10 + 2) 0

duplicate::String ->Int->String
duplicate x y = concat [x | _ <- [1..y]]

conta :: [Char] -> Char -> Int
conta x a = length [n | n <- x, n == a]

converte :: [Int] -> [Char]
converte x = [['A'..'Z'] !! (a-1) | a <- x]

howmany :: Int -> Int -> Int -> Int
howmany a b c = length [x | x <- [b..c], x `mod` a == 0]

insere :: [Int] -> Int -> [Int]
insere x n = [y | y <- x, y<= n] ++ [n] ++ [y | y <- x, y> n]

scalar :: [Int] -> [Int] -> Int
scalar a b = sum [x*y | (x,y) <- zip a b]