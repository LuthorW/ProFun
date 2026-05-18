{- HLINT ignore "Use foldr" -}
{- HLINT ignore "Use map" -}
import Data.Char (isUpper, isLower, isDigit)

--  função que soma todos os elementos da lista de inteiros
somaList :: [Int] -> Int
somaList [] = 0
somaList (x:xs) = x + somaList xs

-- função que soma dois números inteiros
soma :: Int -> Int -> Int
soma x y = x +y

-- função que fala o tamanho de uma lista de inteiros
tamList :: [Int] -> Int
tamList [] = 0
tamList (_:xs) = 1 + tamList xs

-- função que inverte uma lista de inteiros
invertList :: [Int] -> [Int]
invertList [] = []
invertList (x:xs) = invertList xs ++ [x]

-- função que inverte uma lista de listas de inteiros
invertInter :: [[Int]] -> [[Int]]
invertInter [] = []
invertInter (x:xs) = invertList x : invertInter xs

invertInternas :: [[Int]] -> [[Int]] -- outro modo de fazer
invertInternas = map reverse

-- função que calcula o fatorial de um número inteiro
fatorial :: Int -> Int
fatorial 0 = 1
fatorial x = x * fatorial (x-1)

f1 :: Float -> Float
f1 x
    | x>=0 = (x+4)/(x+2)
    | otherwise = 2/x

f2 :: Float -> Float -> Float
f2 x y
    | x >= y = x + y
    | otherwise = x - y

f3 :: Float -> Float -> Float -> Float
f3 x y z
    | (x+y) > z = x + y + z
    | (x+y) < z = x - y - z
    | otherwise = 0


-- funcão para ^2
square :: Int -> Int
square x =  x * x 

-- função para ^4
fourPower :: Int -> Int
fourPower x = square (square x)

-- inverte uma lista usando 'reverse'
invert :: [Int] -> [Int]
invert lista
    | null lista = []
    | otherwise = reverse lista

-- faz uma multiplicação usando a soma
multiSom :: Int -> Int -> Int
multiSom _ 0 = 0
multiSom x y = soma x (multiSom x (y-1))

-- função que confere se tem um elemento na lista
searchList::Int->[Int]->Bool
searchList _ [_] = False
searchList a (x:xs)
    | a == x = True
    | otherwise = searchList a xs

-- função que recebe uma lista e retorna a cabeça 
myHead :: [Int] -> Int
myHead (x:xs) = x

-- função que recebe uma lista e retorna a cauda
myTail :: [Int] -> [Int]
myTail (x:xs) = xs

-- função que recebe uma lista e retorna o último elemento 
myLast :: [Int] -> Int
myLast [x] = x
myLast (_:b) = myLast b

-- função que recebe uma lista e ignora o último elemento
myInit :: [Int] -> [Int]
myInit [a] = []
myInit (x:xs) = x : myInit xs

-- função que retorna o último dígito de um número int
lastDigit :: Int -> Int
lastDigit x = x `mod` 10

-- função que recebe [Char] e retorna [(Bool,Char)] True se Char for alfanumérico e False, caso contrário
setAlfa:: String -> [(Bool, Char)] 
setAlfa [] = []
setAlfa (x:xs) = (meuIsAlphaNum x, x) : setAlfa xs

-- função para verificar se é alfanumérico, sem usar a pré definida
meuIsAlphaNum :: Char -> Bool
meuIsAlphaNum c = isUpper c || isLower c || isDigit c
