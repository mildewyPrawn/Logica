--Logica Computacional
--profesor: Pilar Selene Linares Arevalo
--ayudante: Alejandro Hernandez Mora
--ayudante: Luis Manuel Martinez Damaso
--Practica3
--alumno: Galeana Araujo Emiliano
--No de Cuenta: 314032324
--alumno: Verdugo Rodriguez Jesus Daniel
--No de Cuenta: 417092056
--alumno: Ángeles Martínez Ángela Janín
--No de Cuenta: 314201009

module Practica2 where

data Var = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z
         deriving (Show, Eq, Ord)

data Formula = Prop Var
             |Neg Formula
             |Formula :&: Formula
             |Formula :|: Formula
             |Formula :=>: Formula
             |Formula :<=>: Formula deriving (Show, Eq, Ord)


infixl 9 :&:
infixl 9 :|:
infixr 7 :=>:
infixl 8 :<=>:


--Una función recursiva que recibe una fórmula y devuelve el conjunto (lista sin
--  repeticiones) de variables que hay en la fórmula. HINT: Elimina las
-- repeticiones utilizando una función de tu práctica anterior.
varList :: Formula -> [Var]
varList (Prop p) = [p]
varList (Neg p) = repeticiones(varList(p))
varList (p :&: q) = repeticiones(varList(p) ++ varList(q))
varList (p :|: q) = repeticiones(varList(p) ++ varList(q))
varList (p :=>: q) = repeticiones(varList(p) ++ varList(q))
varList (p :<=>: q) = repeticiones(varList(p) ++ varList(q))

--Una función que recibe una fórmula y devuelve su negación.
negacion :: Formula -> Formula
negacion (Prop p) = (Neg (Prop p))
negacion (Neg p) = (p)
negacion (p :&: q) = (negacion p) :|: (negacion q)
negacion (p :|: q) = (negacion p) :&: (negacion q)
negacion (p :=>: q) = p :&: (negacion q)
negacion (p :<=>: q) =(p :&: (negacion q)) :|: (q :&: (negacion p))

--Una función que recibe una fórmula y elimina implicaciones y equivalencias.
equivalencia :: Formula -> Formula
equivalencia (Prop p) = (Prop p)
equivalencia (Neg p) = (negacion (equivalencia p))
equivalencia (p :&: q) = ((equivalencia p) :&: (equivalencia q))
equivalencia (p :|: q) = ((equivalencia p) :|: (equivalencia q))
equivalencia (p :=>: q) = ((negacion (equivalencia p)) :|: (equivalencia q))
equivalencia (p :<=>: q) = (((negacion(equivalencia p)) :|: (equivalencia q))
                           :&: ((negacion(equivalencia q)) :|: (equivalencia p)))

--Una función recursiva que recibe una fórmula proposicional y una lista de
-- parejas de variables proposicionales. La función debe sustituir todas las
-- presencias de variables en la fórmula por la pareja ordenada que le
-- corresponde en la lista.
sustituye :: Formula -> [(Var,Var)] -> Formula
sustituye (Prop p) [] = (Prop p)
sustituye (Prop p) ((x,y):xs) = if varList(Prop p) == fst ([x],y)
                            then(Prop(snd(x,y)))
                            else sustituye (Prop p) xs
sustituye (Neg p) l1 = negacion(sustituye p l1)
sustituye (p :&: q) l1 = (sustituye p l1) :&: (sustituye q l1)
sustituye (p :|: q) l1 = (sustituye p l1) :|: (sustituye q l1)
sustituye (p :=>: q) l1 = (sustituye p l1) :=>: (sustituye q l1)
sustituye (p :<=>: q) l1 = (sustituye p l1) :<=>: (sustituye q l1)

--Una función recursiva que recibe una fórmula y una lista de parejas ordenadas
-- de variables con estados (True y False) y evalua la fórmula asignando el
-- estado que le corresponde a cada variable. Si existe alguna variable que no
-- tenga estado asignado y sea necesaria para calcular el valor de la
-- proposición, muestra el error: “No todas las variables están definidas"
interp :: Formula -> [(Var,Bool)] -> Bool
interp _ [] = error "ERROR:  Lista vacía D:"
interp (Prop p) ((x,y):xs) = if varList(Prop p) == fst ([x],y)
                            then (snd(x,y))
                            else interp (Prop p) xs
interp (Neg p) l1 = not (interp p l1)
interp (p :&: q) l1 = (interp p l1) && (interp q l1)
interp (p :|: q) l1 = (interp p l1) || (interp q l1)
interp (p :=>: q) l1 = not(interp p l1) || (interp q l1)
interp (p :<=>: q) l1 = interp (p :=>: q) l1 && interp (q :=>: p) l1

--Una función que recibe una fórmula y devuelve la fórmula en Forma normal
  -- negativa. Decimos que una fórmula ψ está en forma normal negativa si y sólo
-- si en ψ las negaciones quedan únicamente frente a fórmulas atómicas y no hay
-- presencias de conectivo de implicación, ni equivalencia.
fnn :: Formula -> Formula
fnn (Prop p) = Prop p
fnn (Neg p) = (negacion (fnn (p)))
fnn (p :&: q) = ((fnn p) :&: (fnn q))
fnn (p :|: q) = ((fnn p) :|: (fnn q))
fnn (p :=>: q) = ((negacion (fnn p)) :|: (fnn q))
fnn (p :<=>: q) = (((negacion (fnn p)) :|: (fnn q))
                   :&: ((negacion (fnn q)) :|: (fnn p)))

--Función recursiva que permite expresar cualquier fórmula proposicional como
-- una conjunción de disyunciones llamadas 'clausulas'.
fnc :: Formula -> Formula
fnc (Prop p) = (Prop p)
fnc (Neg p) = fnc((fnn(Neg p)))
fnc (p :&: q) = fnc(fnn p) :&: fnc(fnn q)
fnc (p :|: q) = distrN(distrN ((fnn p)) :|: distrN(fnn q))
fnc (p :=>: q) = distrN(negacion(fnn p) :|: fnn q)
--fnc (p :=>: q) = fnc(negacion(fnn p) :|: fnn q)
fnc (p :<=>: q) = fnc(p :=>: q) :&: fnc(q :=>: p)

--Función que recibe una fórmula y el resultado es una lista de 2^n pares ordenados.
-- Donde el primer elemento es un estado para cada una de las variables y el segundo es el
-- es resultado de la función de interpretaciones de la formula en ese estado.
tablaVerdad :: Formula -> [([(Var, Bool)], Bool)]
tablaVerdad f = pegar (map (pegar (varList f))(renglones (length(varList f)) [[]]))(auxTV1 f)

--Función que recibe una fórmula y devuelve 'True' si la fórmula es tautología.
-- 'False' en otro caso. Utilizar la función 'tablaVerdad'.
esTautologia :: Formula -> Bool
esTautologia f = foldr (&&) True [x | (_,x) <- tablaVerdad f]

--Función que recibe una fórmula y devuelve 'True' si la fórmula es contradicción.
-- 'False' en otro caso. Utilizar la función 'tablaVerdad'.
esContradiccion :: Formula -> Bool
esContradiccion f = not (foldr (||) False [x | (_,x) <- tablaVerdad f])

--Función que recibe una fórmula y devolve 'True' si la fórmula es satisfacible.
-- 'False' en otro caso. Utilizar la función 'tablaVerdad'.
esSatisfacible :: Formula -> Bool
esSatisfacible f = foldr (||) False [x | (_,x) <- tablaVerdad f]

--Función que calcula el conjunto S de cláusulas de una fórmula.
calculaS :: Formula -> [[Formula]]
calculaS = error "Es solo para que corra"

--Función que recibe dos cláusulas y devuelve el solvent de ambas.
res :: [Formula] -> [Formula] -> [Formula]
res = error "Es para que corra"

--Función que indica si se obtiene la cláusula vacía después de aplicar el
-- algoritmo de saturación a un conjunto de cláusulas.
resolucionBinaria :: Formula -> Bool
resolucionBinaria =  error "Es para que corra"

--Función que recibe un conjunto de premisas, una conclusión y nos dice si el argumento
-- lógico es correcto.
esCorrecto :: [Formula] -> Formula -> Bool
esCorrecto = error "Es para que corra"

----------------------------------------------------------------------
--                        FUNCIONES AUXILIARES                      --
----------------------------------------------------------------------

tf = [True, False]

auxTV1 f = map (interp f) (map(pegar (varList f)) (renglones (length(varList f)) [[]]))

repeticiones :: (Eq a) => [a] -> [a]
repeticiones [] = []
repeticiones (x:xs) = if(elem x xs == True)
                      then repeticiones xs
                      else [x] ++ repeticiones xs

distrN :: Formula -> Formula
distrN ((r :&: s) :|: q) = distrN(q :|: r) :&: distrN(q :|: s)
distrN (q :|: (r :&: s)) = distrN(q :|: r) :&: distrN(q :|: s)
distrN (p :|: q) = p :|: q
distrN (Neg(Prop p)) = Neg(Prop p)
distrN (Prop p) = Prop p

renglones :: Int -> [[Bool]] -> [[Bool]]
renglones 0 ac = ac
renglones n l = renglones (n-1) [(x:y) | x <- tf, y <- l]

pegar [] _ = []
pegar (x:xs) (y:ys) = (x,y):(pegar xs ys)



----------------------------------------------------------------------
--                             PRUEBAS                             --
----------------------------------------------------------------------
