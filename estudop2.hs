
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