--Logica Computacional
--profesor: Pilar Selene Linares Arevalo
--ayudante: Alejandro Hernandez Mora
--ayudante: Luis Manuel Martinez Damaso
--Practica2
--alumno: Galeana Araujo Emiliano
--No de Cuenta: 314032324
--alumno: Verdugo Rodriguez Jesus Daniel
--No de Cuenta: 417092056
--alumno: Ángeles Martínez Ángela Janín
--No de Cuenta: 314201009

module Practica2 where
--declaracion de los tipo?

data Var = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z
         deriving (Show, Eq, Ord)
data Formula = Prop Var
             --Agregar V,F para True y False (?)
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
sustituye = error "soloes para que interprete"

--Una función recursiva que recibe una fórmula y una lista de parejas ordenadas
-- de variables con estados (True y False) y evalua la fórmula asignando el
-- estado que le corresponde a cada variable. Si existe alguna variable que no
-- tenga estado asignado y sea necesaria para calcular el valor de la
-- proposición, muestra el error: “No todas las variables están definidas"
interp :: Formula -> [(Var,Bool)] -> Bool
interp = error "solo es para que interprete"

--Una función que recibe una fórmula y devuelve la fórmula en Forma normal
-- negativa. Decimos que una fórmula ψ está en forma normal negativa si y sólo
-- si en ψ las negaciones quedan únicamente frente a fórmulas atómicas y no hay
-- presencias de conectivo de implicación, ni equivalencia.
fnn :: Formula -> Formula
fnn (Prop p) = Prop p
fnn (Neg p) = negacion (fnn (p))
fnn (p :&: q) = ((fnn p) :&: (fnn q))
fnn (p :|: q) = ((fnn p) :|: (fnn q))
fnn (p :=>: q) = ((negacion (fnn p)) :|: (fnn q))
fnn (p :<=>: q) = (((negacion (fnn p)) :|: (fnn q))
                   :&: ((negacion (fnn q)) :|: (fnn p)))

--Función recursiva que permite expresar cualquier fórmula proposicional como
-- una conjunción de disyunciones llamadas 'clausulas'.
fnc :: Formula -> Formula
fnc (Prop p) = Prop p
fnc (Neg p) = negacion (fnc (fnn p))
fnc (p :&: q) = (fnc (fnn p) :&: fnc (fnn q))
fnc (p :|: q) = fnc (fnn p) :|: fnc (fnn q)
fnc (p :=>: q) = negacion (fnc (fnn p)) :|: fnc (fnn q)
fnc (p :<=>: q) =(negacion p :|: q) :&: (negacion q :|: p) 

--fnc = error "esto solo es para que interprete"

----------------------------------------------------------------------
--                        FUNCIONES AUXILIARES                      --
----------------------------------------------------------------------

repeticiones :: (Eq a) => [a] -> [a]
repeticiones [] = []
repeticiones (x:xs) = if(elem x xs == True)
                      then repeticiones xs
                      else [x] ++ repeticiones xs

----------------------------------------------------------------------
--                             PRUEBAS                             --
----------------------------------------------------------------------



