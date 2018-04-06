-- | Logica Compputacional 2018-2
-- | Practica 3
-- | Profesor: Pilar Selene Linares Arévalo
-- | Laboratorio: Alejandro Hernández Mora
-- | Integrantes:
-- | Galeana Araujo Emiliano 314032324 galeanaara@ciencias.unam.mx
-- | Licon Colon Francisco Arturo  314222095  franciso17@ciencias.unam.mx
-- | Molina Bis Victor Hugo  314311212  victor98@ciencias.unam.mx

module Practica3 where
import Data.List

data Var = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z deriving (Show, Eq, Ord)
data Formula = Const Bool |Prop Var| Neg Formula| Formula :&: Formula| Formula :|: Formula| Formula :=>: Formula| Formula :<=>: Formula deriving (Show, Eq, Ord)
infixl 9 :&:
infixl 9 :|:
infixl 7 :=>:
infixl 8 :<=>:

-- Funcion que recibe una formula y devuelve el conjunto sin repeticiones de variables que hay en la formula

--Funcion auxiliar que elimina elementos repetidos de una lista
eliminaDup :: (Eq a) => [a] -> [a]
eliminaDup [] = []
eliminaDup (x:xs) | elem x xs    = eliminaDup xs
                  |otherwise = [x] ++ eliminaDup xs

varList :: Formula -> [Var]
varList x = eliminaDup(varsHelper x)
    where varsHelper (Prop a) = [a]
          varsHelper (Neg f) = varsHelper f
          varsHelper (f :&: g) = (varsHelper f) ++ (varsHelper g)
          varsHelper (f :|: g) = (varsHelper f) ++ (varsHelper g)
          varsHelper (f :=>: g) = (varsHelper f) ++ (varsHelper g)
          varsHelper (f :<=>: g) = (varsHelper f) ++ (varsHelper g)

type Interpretacion = [(Var,Bool)]

--Valor de la proposición p en la interpretación i.
valor :: Interpretacion -> Formula -> Bool
valor _ (Const b) = b
valor i (Prop x) = fi x i
valor i (Neg p) = not (valor i p)
valor i (p :&: q) = valor i p && valor i q
valor i (p :|: q) = valor i p || valor i q
valor i (p :=>: q) = valor i p <= valor i q
valor i (p :<=>: q) = valor i p == valor i q

--fi(x xs)
fi :: Eq c => c -> [(c,v)] -> v
fi c r = head [v | (m,v) <- r, c == m]

--Lista de interpretaciones de las variables.
interpVar :: Int -> [[Bool]]
interpVar 0 = [[]]
interpVar n =
    [False:xs | xs <- xss] ++ [True:xs | xs <- xss]
    where xss = interpVar (n-1)

interp :: Formula -> [Interpretacion]
interp p = [zip xs i | i <- ys]
           where xs = varList p
                 ys = interpVar (length xs)

-- 1. Tabla de verdad.
tablaVerdad :: Formula -> [([(Var,Bool)],Bool)]
tablaVerdad f = [(x,y) | x <- interp f, y <- [valor x f]]

-- 2. Función que recibe una fórmula, devuelve True si es tautología, False en otro caso.
esTautologia :: Formula -> Bool
esTautologia p = and [y | (x,y) <- tablaVerdad p]

-- 3. Función que recibe una fórmula, devuelve True si es contradiccion, False en otro caso.

contradiccion :: [([(Var,Bool)],Bool)] -> Bool
contradiccion [] = True
contradiccion ((x,y):xs) = if y == True
                           then False
                           else (contradiccion xs)

esContradiccion :: Formula -> Bool
esContradiccion p = contradiccion(tablaVerdad p)

-- 4. Función que recibe una fórmula y devuelve True, si es satisfacible, False en otro caso.
esSatisfacible :: Formula -> Bool
esSatisfacible p = or [y | (x,y) <- tablaVerdad p]

-- 5. Funcion que calcula el conjunto S de cláusulas de una fórmula

clausula :: Formula -> [Formula]
clausula (Prop f) = [Prop f]
clausula (Neg f) = [Neg f]
clausula (f :|: g) = clausula f ++ clausula g

calculaS :: Formula -> [[Formula]]
calculaS f =
  let g = fnc(f)
  in cal(g)

cal :: Formula -> [[Formula]]
cal (f :&: g) = cal f ++ cal g
cal f = [clausula f]

distributividad :: Formula -> Formula
distributividad ((p :&: q) :|: r) = distributividad(((distributividad p) :|: (distributividad r)):&: ((distributividad q) :|: (distributividad r)))
distributividad (r :|: (p :&: q)) = distributividad(((distributividad r) :|: (distributividad p)):&: ((distributividad r) :|: (distributividad q)))
distributividad (f :&: g) = (distributividad f) :&: (distributividad g)
distributividad f = f

fnc :: Formula -> Formula
fnc f = distributividad(fnn f)

interiorizaNegacion :: Formula -> Formula
interiorizaNegacion (Prop p) = Prop p
interiorizaNegacion (Neg p) = interiorizaNegacionAux p
interiorizaNegacion (p :&: q) = (interiorizaNegacion p) :&: (interiorizaNegacion q)
interiorizaNegacion (p :|: q) = (interiorizaNegacion p) :|: (interiorizaNegacion q)

interiorizaNegacionAux :: Formula -> Formula
interiorizaNegacionAux (Prop p) = Neg (Prop p)
interiorizaNegacionAux (Neg p) = interiorizaNegacion p
interiorizaNegacionAux (p :&: q) = (interiorizaNegacionAux p) :|: (interiorizaNegacionAux q)
interiorizaNegacionAux (p :|: q) = (interiorizaNegacionAux p) :&: (interiorizaNegacionAux q)

fnn :: Formula -> Formula
fnn p = interiorizaNegacion (equivalencia p)

equivalencia :: Formula -> Formula
equivalencia (Prop a) = (Prop a)
equivalencia (Neg f) = (Neg (equivalencia f))
equivalencia (f :&: g) = (equivalencia f :&: equivalencia g)
equivalencia (f :|: g) = (equivalencia f :|: equivalencia g)
equivalencia (f :=>: g) = ((negacion(equivalencia f)) :|: equivalencia g)
equivalencia (f :<=>: g) = ((equivalencia (f :=>: g)) :&: (equivalencia (g :=>: f)))

negacion :: Formula -> Formula
negacion (Prop a) = (Neg (Prop a))
negacion (Neg f) = f
negacion (f :&: g) = (negacion f :|: negacion g)
negacion (f :|: g) = (negacion f :&: negacion g)
negacion (f :=>: g) = ((negacion f) :|: g)
negacion (f :<=>: g) = (negacion((negacion(f :=>: g)) :&: (negacion(g :=>: f))))

-- 6. Función que recibe dos cláusulas y devuelve el resolvente de ambas.
res :: [Formula] -> [Formula] -> [Formula]
res f [] = f
res [] f = f
res (f:fs) (g:gs) =
  let h = (f:fs) ++ (g:gs)
      e = resAux (f:fs) (g:gs)
  in if length(e) == 2
  then repeticiones(eliminaList e h)
  else if length(e) == 0
       then h
       else repeticiones(eliminaList (drop ((length e)-2) e) h)

-- función recursiva que regresa una lista con las variables a eliminar
resAux :: [Formula] -> [Formula] -> [Formula]
resAux [] f = []
resAux (f:fs) (g:gs) = listaForm f (g:gs) ++ resAux fs (g:gs)

-- recibe formula f, y una lista l; regresa vacio si f no esta en l, y [f] en otro caso
listaForm :: Formula -> [Formula] -> [Formula]
listaForm f (x:xs) = if ((negacion f) `elem` (x:xs))
                      then [(negacion f)] ++ [f]
                      else []

-- funcion que elimina un elemento de una lista.
elimina :: (Eq a) => a -> [a] -> [a]
elimina a [] = []
elimina a (x:xs) = if a == x
                   then elimina a xs
                   else [x] ++ elimina a xs

-- funcion que elimina una lista de otra, elimina cada elemento de la lista1 en la l2.
eliminaList :: (Eq a) => [a] -> [a] -> [a]
eliminaList f [] = f
eliminaList [] f = f
eliminaList (f:fs) (g:gs) = eliminaList fs (elimina f (g:gs))

-- funcion que regresa una lista canónica.
repeticiones :: (Eq a) => [a] -> [a]
repeticiones [] = []
repeticiones (x:xs) = if(elem x xs == True)
                      then repeticiones xs
                      else [x] ++ repeticiones xs

-- 7. Una función que indica si se obtiene la cláusula vacía después de aplicar el algoritmo de saturación a un conjunto de cláusulas.
resolucionBinaria :: Formula -> Bool
resolucionBinaria f =
  let cSf = calculaS f
      resN = conjunto cSf
  in if [] `elem` resN
     then False
     else True

conjunto [] = []
conjunto (x:xs) = map (res x) (xs) ++ conjunto xs


-- 8. Función que recibe un conjunto de premisas y una conclusión y nos dice si el argumento lógico es correcto.
esCorrecto :: [Formula] -> Formula -> Bool
esCorrecto (x:xs) p = esTautologia(esCorrectoHelper (x:xs) :=>: p)

esCorrectoHelper :: [Formula] -> Formula
esCorrectoHelper (x:_) = x
esCorrectoHelper (x:xs) = (x :&: esCorrectoHelper xs)
