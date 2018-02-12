{-
- Lógica Computacional 2018-2
- Práctica1.
- Galeana Araujo Emiliano
- Verdugo Rodriguez Jesús Daniel
-}

module Practica1 where

--Función que evalua la derivada de la función f(x)= ax^2+bx+c.
deriva:: Int -> Int -> Int -> Int -> Int
deriva a b c x = ((2*a)*x) + b

--Función que calcula el área de un cilindro.
areaCilindro:: Float -> Float -> Float
areaCilindro r h = ((2*(3.14159))*r)*(r+h)

--Función que calcula el volumen de un cilindro-
volumenCilindro:: Float -> Float -> Float
volumenCilindro r h = ((areaCirculo r) * h)

--Función auxiliar para calcular el área de un círculo
areaCirculo:: Float -> Float
areaCirculo r = ((3.14159)*(r*r))

--Función que calcula distintas operaciones dados tres parámetros.
aplicaOperacion:: Char -> Int -> Int -> Int
aplicaOperacion c n1 n2
  | c == 's' = n1
  | c == 't' = n2
  | c == 'a' = n1+n2
  | c == 'r' = n1-n2
  | c == 'p' = n1*n2
  | c == 'd' = n1 `div` n2
  | c == 'e' = expo n1 n2
  | otherwise = error "No es un caracter valido "

--Función auxiliar que calcula el valor de un número elevado al segundo.
expo:: Int -> Int -> Int
expo n 0 = 1
expo 0 n = 0
expo n m = ((expo n (m-1))*n)

--Función que devuelve la aproximación entera de su raíz cuadrada.
raizEntera:: Int -> Int
raizEntera x =  round(sqrt(realToFrac x))

--Función que devuelve la suma de los primeros n numeros naturales.
sumaNat:: Int -> Int
sumaNat 0 = 0
sumaNat n = n + (sumaNat (n-1))

--Función Que devuelve la lista con los primeros n números de la
-- sucesión de tribonacci.
tribonaccies:: Int -> [Int]
tribonaccies n = map tribo [0..n]

--tribonaccies n = [tribo n | n <- [0..n]]

--Función auxiliar que calcula un número de tribonacci.
tribo:: Int -> Int
tribo 0 = 0
tribo 1 = 1
tribo 2 = 1
tribo n = (tribo(n-1))+(tribo(n-2))+(tribo(n-3))

--Función que recibe una lista y elimina los duplicados adyacentes de la lista.
eliminaDup:: [a] -> [a]
eliminaDup [] = []
eliminaDup (x:xs) = eliminaDupAux x (xs)

eliminaDupAux y [] = [y]
eliminaDupAux y (x:xs) = if y == x then eliminaDupAux y xs else eliminaDupAux x xs
--Función que regresa la reversa de una lista.
reversa:: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa(xs) ++ [x]
{--
--Función que devuelve una lista con los elementos que cumplan el 'filter'.
filtra:: (a -> Bool) -> [a] -> [a]
--}
--Función que devuelve la lista con pares (k,x) donde k es el máximo numero de
-- n apariciones consecutivas de x.
--apariciones:: [a] -> [(Int, a)]

------------------------------------
--           Ejercicio 9          --
------------------------------------

lista1 = [x-1 | x <- [2^y | y <- [0..6]]]
--Resultado: [0,1,3,7,15,31,63]
lista2 = zip [(x*4)-1 | x <- [1..]] [x*4 | x <- [1..]]
--Resultado: [(3,4),(7,8),(11,12),(15,16)]


deriva1 = deriva 4 10 20 5
--Resultado: 50
deriva2 = deriva 2 3 4 2
--Resultado: 11
areaCilindro1 = areaCilindro 10 20
--Resultado: 1884.95
areaCilindro2 = areaCilindro 8 6
--Resultado: 703.71
volumenCilindro1 = volumenCilindro 10 7
--Resultado: 2199.11
volumenCilindro2 = volumenCilindro 2 44
--Resultado: 552.91
aplicaOperacionS = aplicaOperacion 's' 32 0
--Resultado: 32
aplicaOperacionT = aplicaOperacion 't' 0 23
--Resultado: 23
aplicaOperacionA = aplicaOperacion 'a' 10 10
--Resultado: 20 
aplicaOperacionR = aplicaOperacion 'r' 15 20
--Resultado: -5
aplicaOperacionP = aplicaOperacion 'p' 4 3
--Resultado: 12
aplicaOperacionD = aplicaOperacion 'd' 8 3
--Resultado: 2
aplicaOperacionE = aplicaOperacion 'e' 2 3
--Resultado: 8
raizEntera1 = raizEntera 4
--Resultado: 2
raizEntera2 = raizEntera 110
--Resultado: 10
sumaNat1 = sumaNat 15
--Resultado: 120
sumaNat2 = sumaNat 19456
--Resultado: 189277696
tribonaccies1 = tribonaccies 4
--Resultado: [0,1,1,2,4]
tribonaccies2 = tribonaccies 17
--Resultado: [0,1,1,2,4,7,13,24,44,81,149,274,504,927,1705,3136,5768,10609]
--eliminaDup1
--Resultado:
--eliminaDup2
--Resultado
reversa1 = reversa [0,1,2,3,4,5,6,7,8,9]
--Resultado: [9,8,7,6,5,4,3,2,1,0]
reversa2 = reversa ["a","b","c","d","e"]
--Resultado: ["e","d","c","b","a"]
--filtra1
--Resultado:
--filtra2
--Resultado:
--apariciones1
--Resultado:
--apariciones2
--Resultado:
