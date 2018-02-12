--Logica Computacional
--profesor: Pilar Selene Linares Arevalo
--ayudante: Alejandro Hernandez Mora
--ayudante: Luis Manuel Martinez Damaso
--Practica1
--alumno: Galeana Araujo Emiliano
--alumno: Verdugo Rodriguez Jesus Daniel
--No de Cuenta: 314032324
--No de Cuenta: 417092056

module Practica1 where

--Funcion que recibe cuatro enteros y calcula la derivada de la funcion f(x) =ax^2 + bx + c
--el primer elemento corresponde a a, el segundo a b, el tercero a c yel cuarto a x
deriva :: Int -> Int -> Int -> Int -> Int
deriva a b c x = 2*a*x + b

--funcion que recibe dos numeros de punto flotante, elprimer parametro corresponde al radio de un cilindro, el segundo a la altura
--Area del cilindro
areaCilindro :: Float -> Float -> Float
areaCilindro a b = 2*3.1416*a*(a+b)

--Volumen de un cilindro.
volumenCilindro :: Float -> Float -> Float
volumenCilindro a b = 3.1416*a*a*b

--funcion que recibe tres parametros, el primero indica la operacion que se desea realizar,
--el segundo un numero y el tercero otro numero, las operaciones que se pueden hacer son:
--'s'-Devuelve el segundo parametro
--'t'-Devuelve el tercer parametro
--'a'-Devuelve la suma de ambos numeros
--'r'-Devuelve la resta de ambos numeros
--'p'-Devuelve el producto de ambos numeros
--'d'-Devuelve la division de ambos numeros
--'e'-Devuelve la potencia(segundo parametro elevado al tercer parametro)
aplicaOperacion :: Char -> Int -> Int -> Int
aplicaOperacion z x y
  | z == 's' = x
  | z == 't' = y
  | z == 'a' = x+y
  | z == 'r' = x-y
  | z == 'p' = x*y
  | z == 'd' = div x y
  | z == 'e' = x^y
  | otherwise = error "opcion no valida, vuelve a intentarlo con una ocion valida"

--funcion que reciba un entero y devuelva la proxima entrada a su raiz cuadrada
raizEntera :: Int -> Int
raizEntera x = round (sqrt (realToFrac x))

--funcion que recibe un entero n y devuelve la suma de los primeros n numeros naturales
sumaNat :: Int -> Int
sumaNat 0 = 0
sumaNat n = n + (sumaNat (n-1))

--funciones de orden superior
--recibe un numero n y devuelve los primeros n terminos de la sucecion tribonacci que inicia con [0,1,1].
tribonaccies :: Int -> [Int]
tribonaccies n = map fi [0..n]

--funcion que recibe una lista y elimina los duplicados adyacentes de la lista, dejando una presencia de cada elemento
eliminaDup :: (Eq a) => [a] ->  [a]
eliminaDup [] = []
eliminaDup (x:xs) = eliminaAux x xs

--operaciones con listas
--funcion que recibe una lista y devuelve la misma pero en el orden inverso
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa(xs) ++ [x]

--recibe una lista y devuelve los elementos de la lista que cumplen el predicado recibido
filtra :: (a -> Bool) -> [a] -> [a]
filtra = error "error"

--recibe una lista y devuelve una lista con pares ordenados (k,x), donde k es el maximo numero de apariciones
--consecutivas del elemento x.
apariciones :: [a] -> [(Int,a)]
apariciones = error "solo es para Que compile"

--listas por comprencion
lista1 = [x-1 | x <- [2^y | y <- [0..6]]]
--Regresa: [0,1,3,7,15,31,63]
lista2 = zip [(x*4)-1 | x <- [1..]] [x*4 | x <- [1..]]
--Regresa: [(3,4),(7,8),(11,12),(15,16),...]

----------------------------------------------------------------------------------
--                         FUNCIONES AUXILIARES                                 --
----------------------------------------------------------------------------------

--Función auxiliar que calcula un número de tribonacci.
fi :: Int -> Int
fi 0 = 0
fi 1 = 1
fi 2 = 1
fi n = (fi(n-1))+(fi(n-2))+(fi(n-3))

--función que elimina los duplicados adyacentes de una lista, empezando por la cabeza.
eliminaAux :: (Eq a) => a -> [a]  -> [a]
eliminaAux y [] = [y]
eliminaAux y (x:xs) = if (y == x)
                      then eliminaAux y xs 
                      else [y] ++ eliminaAux x xs

--función que elimina los duplicados adyacentes de una lista, empezando por la cabeza.
rep :: (Eq a) => [a] -> [a]
rep [] = []
rep (x:xs) = if((length (x:xs)) > 1)
                    then if(x == xs !! 0)
                         then rep xs
                         else [x] ++ rep xs
                    else [x]

----------------------------------------------------------------------------------
--                                 PRUEBAS                                      --
----------------------------------------------------------------------------------

deriva1 = deriva 4 10 20 5
--Resultado: 50
deriva2 = deriva 2 3 4 2
--Resultado: 11
areaCilindro1 = areaCilindro 10 20
--Resultado: 1884.96
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
eliminaDup1 = eliminaDup [1,1,1,1,1,1,2,2,2,2,2,3,3,3,3,4,4,4,5,5,5,6,7,7,7,7,8,8,8,8,9,9,9,0,0,0]
--Resultado: [1,2,3,4,5,6,7,8,9,0]
eliminaDup2 = eliminaDup [1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2]
--Resultado: [1,2,1,2,1,2,1,2,1,2]
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
