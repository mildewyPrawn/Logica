module Practica1 where

--Logica Computacional
--profesor: Pilar Selene Linares Arevalo
--ayudante: Alejandro Hernandez Mora
--ayudante: Luis Manuel Martinez Damaso
--Practica1
--alumno: Verdugo Rodriguez Jesus Daniel
--No de Cuenta:

--Funcion que recibe cuatro enteros y calcula la derivada de la funcion f(x) =ax^2 + bx + c
--el primer elemento corresponde a a, el segundo a b, el tercero a c yel cuarto a x
deriva :: Int -> Int -> Int -> Int -> Int
deriva a b c x = a*2*x + b*x

--funcion que recibe dos numeros de punto flotante, elprimer parametro corresponde al radio de un cilindro, el segundo a la altura
--Area del cilindro
areaCilindro :: Float -> Float -> Float
areaCilindro a b = 2*3.1416*a*(a+b)

--Volumen del cilindro
volumenCilindro :: Float -> Float -> Float
volumenCilindro a b = 2*3.1416*a*a*b

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
sumaNat n = n + (sumaNat n-1)

--funciones de orden superior
--recibe un numero n y devuelve los primeros n terminos de la sucecion tribonacci que inicia con [0,1,1].
tribonaccies :: Int -> [Int]
tribonaccies n = map fi [0..n]
--tribonaccies = error "solo es para que compile"
--recibe una lista y elimina los duplicados adyacentes de la lista, dejando una presencia de cada elemento
eliminaDup :: [a] -> [a]
eliminaDup (x:xs) = xs

prueba = rep [4,1,1,1,1,23,23,23,4]

---funcion auxiliar que recibe una lista y te dice si el primer elemento es igual al segundo 
rep :: (Eq a) => [a] -> [a]
rep [] = []
rep (x:y:xs) = if(x == y)
                      then rep (y:xs)
                      else [x] ++ rep (y:xs)
--operaciones con listas
--funcion que recibe una lista y devuelve la misma pero en el orden inverso
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa(xs) ++ [x]
--recibe una lista y devuelve los elementos de la lista que cumplen el predicado recibido
filtra :: (a -> Bool) -> [a] -> [a]
filtra = error "error"
--filtra x:xs = x
--recibe una lista y devuelve una lista con pares ordenados (k,x), donde k es el maximo numero de apariciones
--consecutivas del elemento x.
apariciones :: [a] -> [(Int,a)]
apariciones = error "solo es para que compile"

--listas por comprencion
--here
--here
--here
--here



----------------------------------------------------------------------------------
--FUNCIONES
----------------------------------------------------------------------------------
--Función auxiliar que calcula un número de tribonacci.
fi :: Int -> Int
fi 0 = 0
fi 1 = 1
fi 2 = 1
fi n = (fi(n-1))+(fi(n-2))+(fi(n-3))


--pruebas o cosas para probar el programa
--p
--p

--p

--p

--p
