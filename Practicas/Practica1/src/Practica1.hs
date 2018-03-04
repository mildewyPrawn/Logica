--Logica Computacional
--profesor: Pilar Selene Linares Arevalo
--ayudante: Alejandro Hernandez Mora
--ayudante: Luis Manuel Martinez Damaso
--Practica1
--alumno: Galeana Araujo Emiliano
--No de Cuenta: 314032324
--alumno: Verdugo Rodriguez Jesus Daniel
--No de Cuenta: 417092056
--alumno: Ángeles Martínez Ángela Janín 
--No de Cuenta: 314201009 


module Practica1 where

--Función que recibe cuatro enteros y calcula la derivada de la Función f(x) =ax^2 + bx + c
--el primer elemento corresponde a a, el segundo a b, el tercero a c yel cuarto a x
deriva :: Int -> Int -> Int -> Int -> Int
deriva a b c x = 2*a*x + b

--Función que recibe dos numeros de punto flotante, elprimer parametro corresponde al radio de un cilindro, el segundo a la altura
--Area del cilindro
areaCilindro :: Float -> Float -> Float
areaCilindro a b = 2*3.1416*a*(a+b)

--Volumen de un cilindro.
volumenCilindro :: Float -> Float -> Float
volumenCilindro a b = 3.1416*a*a*b

--Función que recibe tres parametros, el primero indica la operacion que se desea realizar,
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

--Función que reciba un entero y devuelva la proxima entrada a su raiz cuadrada
raizEntera :: Int -> Int
raizEntera n = auxRaizEntera n 1

--Función que recibe un entero n y devuelve la suma de los primeros n numeros naturales
sumaNat :: Int -> Int
sumaNat 0 = 0
sumaNat n = n + (sumaNat (n-1))

--Funciónes de orden superior
--recibe un numero n y devuelve los primeros n terminos de la sucecion tribonacci que inicia con [0,1,1].
tribonaccies :: Int -> [Int]
tribonaccies n = map fi [0..n]

--Función que recibe una lista y elimina los duplicados adyacentes de la lista, dejando una presencia de cada elemento
eliminaDup :: (Eq a) => [a] ->  [a]
eliminaDup [] = []
eliminaDup (x:xs) = eliminaAux x xs

--operaciones con listas
--Función que recibe una lista y devuelve la misma pero en el orden inverso
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa(xs) ++ [x]

--recibe una lista y devuelve los elementos de la lista que cumplen el predicado recibido
filtra :: (a -> Bool) -> [a] -> [a]
filtra p xs = [x | x <- xs, p x]

--recibe una lista y devuelve una lista con pares ordenados (k,x), donde k es el maximo numero de apariciones
--consecutivas del elemento x.
apariciones :: (Eq a) => [a] -> [(Int,a)]
apariciones [] = []
apariciones (x:xs) = repeticiones([(cuentaMax 0 x (x:xs),x)] ++ apariciones(findOther x (x:xs)))

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

--Función que elimina los duplicados adyacentes de una lista, empezando por la cabeza.
eliminaAux :: (Eq a) => a -> [a]  -> [a]
eliminaAux y [] = [y]
eliminaAux y (x:xs) = if (y == x)
                      then eliminaAux y xs 
                      else [y] ++ eliminaAux x xs

--Función que ayuda a calcular la aproximación de una raíz cuadrada.
auxRaizEntera :: Int -> Int -> Int
auxRaizEntera r n = if n*n > r
                    then n-1
                    else auxRaizEntera r (n+1)
cuentaMax :: (Eq a) => Int -> a -> [a] -> Int
cuentaMax n e [] = n
cuentaMax n e (x:xs) = if e == x
                       then cuentaMax (n+1) e xs
                       else if n > cuentaMax 0 e (xs)
                            then n
                            else cuentaMax 0 e (xs)

findNext :: (Eq a) => a -> [a] -> [a]
findNext e [] = []
findNext e (x:xs) = if x == e
                then x:xs
                else xs
findOther :: (Eq a) => a -> [a] -> [a]
findOther y [] = []
findOther y (x:xs) = if x == y
                     then findOther y xs
                     else x:xs

repeticiones :: (Eq a) => [a] -> [a]
repeticiones [] = []
repeticiones (x:xs) = if(elem x xs == True)
                      then repeticiones xs
                      else [x] ++ repeticiones xs

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
eliminaDup1 = eliminaDup [1,1,1,1,1,1,2,2,2,2,2,3,3,3,3,4,4,4,5,5,5,6,7,7
                         ,7,7,8,8,8,8,9,9,9,0,0,0]
--Resultado: [1,2,3,4,5,6,7,8,9,0]
eliminaDup2 = eliminaDup [1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2,1,1,2,2]
--Resultado: [1,2,1,2,1,2,1,2,1,2]
reversa1 = reversa [0,1,2,3,4,5,6,7,8,9]
--Resultado: [9,8,7,6,5,4,3,2,1,0]
reversa2 = reversa ["a","b","c","d","e"]
--Resultado: ["e","d","c","b","a"]
filtra1 = filtra (<5) [1,2,3,4,5,6,7,8,9,0]
--Resultado: [1,2,3,4,0']
filtra2 = filtra (/="a") ["a","b","a","c","a","d","e"]
--Resultado: ["b","c","d","e"]
apariciones1 = apariciones [1,1,1,2,2,3,1,1,1,1,5,5,2,2,2,2,2,6,3,3]
--Resultado: [(4,1),(2,5),(5,2),(1,6),(2,3)]
apariciones2 = apariciones ['a','a','a','b','b','c','a','a','a','a','e','e'
                           ,'b','b','b','b','b','f','c','c']
--Resultado: [(4,'a'),(2,'e'),(5,'b'),(1,'f'),(2,'c')]
