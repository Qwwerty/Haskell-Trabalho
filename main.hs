--  Escreva a função calcCirc para calcular a área de um círculo de raio r. A área de um
--  círculo é dada por: area = π * r 2 ;

calcCirc :: Float -> Float
calcCirc area = 3.14 * (area * area)

--  Escreva uma função que recebe dois números e retorne o menor entre eles;
menorValor :: Int -> Int -> Int
menorValor x y
    | (x > y) = y
    | (y > x) = x


--  Escreva uma função que recebe uma lista de inteiros e retorne o menor elemento da lista;
menorLista :: [Int] -> Int
menorLista [cabeca] = cabeca
menorLista (cabeca:calda)
    | (cabeca < menorLista calda) = cabeca
    | otherwise = menorLista calda

--  Escreva uma função que recebe um valor numérico e devolva o valor 1 se o valor for maior
--  que zero, -1 se for negativo, e 0 se for igual a zero;

maiorQueZero :: Int -> IO()
maiorQueZero x
    | x > 0 = putStrLn "1"
    | x == 0 = putStrLn "0"
    | x < 0 = putStrLn "-1"



--  Escreva uma função que retorna à sequência Fibonnacci até o 10° elemento;
fiboDez :: Int
fiboDez  = fibo (10)

--  Escreva uma função que receba como argumento o índice de um elemento da sequência
--  Fibonnacci e retorne o seu valor;
fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n - 1) + fibo (n - 2)

--  Escreva uma função que receba um inteiro n e retorne uma lista de inteiros
--  correspondente à sequência Fibonnacci até o índice n;
fiboList :: Int -> [Int]
fiboList 0 = [0]
fiboList 1 = [1]
fiboList n = (head(fiboList (n - 1)) + head(fiboList (n - 2))):fiboList(n - 1)

--  Escreva uma função que receba como parâmetro um inteiro e uma lista e retorne
--  verdadeiro caso o inteiro passado esteja contido dentro da lista;
contidoLista :: [Int] -> Int -> Bool
contidoLista [] n = False
contido (a:xs) n 
    | (a /= n) = contidoLista xs n
    | otherwise = (a == n)



--  Escreva uma ou mais funções para ordenar uma lista. O usuário deve informar uma lista
--  de inteiros como argumentos e deve ser retornado uma lista com os mesmos elementos
--  ordenados de forma crescente.
inserir :: Int -> [Int] -> [Int]
inserir x [] = x:[]
inserir x (h:t)
    | x <= h = x:(h:t)
    | otherwise = h:inserir x t

ordenarLista :: [Int] -> [Int]
ordenarLista [] = []
ordenarLista (h:t) = inserir h (ordenarLista t)


--  Escreva uma função em haskell que receba duas listas como argumentos ( [a1,a2,...an] e
--  [b1,b2,...,bn] ) e retorne uma lista com os elementos das duas listas intercalados (
--  [a1,b1,a2,b2,...,an,bn] ).

intercala :: [Int] -> [Int] -> [Int]
intercala [] [] = []
intercala x [] = x
intercala [] y = y
intercala (a:b) (c:d) = (a:[] ++ c:[]) ++ intercala b d