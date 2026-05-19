{- HLINT ignore "Use foldr" -}
{- HLINT ignore "Use map" -}
import Data.Char (isUpper, isLower, isDigit, isNumber, ord)

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
setAlfa :: String -> [(Bool, Char)] 
setAlfa [] = []
setAlfa (x:xs) = (meuIsAlphaNum x, x) : setAlfa xs

-- função para verificar se é alfanumérico, sem usar a pré definida
meuIsAlphaNum :: Char -> Bool
meuIsAlphaNum c = isUpper c || isLower c || isDigit c

-- função que recebe [Char] e retorna [(Bool,Char)] True se Char for númerico
setNum :: String -> [(Bool, Char)]
setNum [] = []
setNum (x:xs) = (isNumber x, x) : setNum xs

-- função que recebe [Char] e retorna o 'endereço' dele na tabela ASCII
alfaInt :: String -> [Int]
alfaInt [] = []
alfaInt (x:xs) = ord x  : alfaInt xs
-- outra maneira de fazer usando 'map'
alfaIntM :: String -> [Int]
alfaIntM = map ord 

-- função que ordena uma lista de inteiros
ordenaLista :: [Int] -> [Int]
ordenaLista [] = []
ordenaLista (x:xs) = insere x (ordenaLista xs)

-- função que insere um número numa lista
insere :: Int -> [Int] -> [Int]
insere a [] = [a]
insere a (x:xs) 
    | a <= x = a : x :xs
    | otherwise = x : insere a xs

-- função que inverte um número inteiro
invertInt :: Int -> Int
invertInt x
  | x < 0     = - (inverteAux 0 (-x))
  | otherwise = inverteAux 0 x

-- Função auxiliar que faz a inversão matemática 
inverteAux :: Int -> Int -> Int
inverteAux acc 0 = acc
inverteAux acc x = inverteAux (acc * 10 + (x `mod` 10)) (x `div` 10)

-- questão 6 da lista 
raizIne :: Int -> Float
raizIne 0 = sqrt 6
raizIne x = sqrt (6 + raizIne(x - 1))

-- questão 7 da lista
comb :: Int -> Int -> Int
comb x y = fatorial x `div` (fatorial y * fatorial (x-y))

-- função que calcula o mdc de dois números
mdc :: Int -> Int -> Int
mdc x y
    | x `mod` y == 0 = y
    | otherwise = mdc y (x `mod` y)







{- TESTE DE VENDAS -}

-- tabela de vendas
vendas :: Int -> Int
vendas 0 = 0
vendas 1 = 41
vendas 2 = 72
vendas 3 = 48
vendas 4 = 0
vendas 5 = 91
vendas 6 = 55
vendas 7 = 30

-- função que retorna uma lista de vendas
listaVendas :: Int-> [Int]
listaVendas 0 = []
listaVendas x = vendas x : listaVendas (x - 1)

-- função que retorna [[Int]] com listas de dia e venda
listaDia :: Int->[[Int]]
listaDia 0 = []
listaDia x = [x, vendas x] : listaDia (x - 1)
