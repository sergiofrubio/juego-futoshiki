module Futoshiki where

import Data.List (sort, transpose)

-- Bidimensional Matrix --
type Matrix a = [[a]]

-- Relation between cells --
data Relation = Ind | RGT | RLT
  deriving (Eq, Read, Show)

-- Futoshiki definition --
data Futoshiki = Futoshiki {
  size  :: Int,
  cells :: Matrix Int,
  hRel  :: Matrix Relation,
  vRel  :: Matrix Relation
}
  deriving (Eq, Read, Show)

-- Función de backtracking genérica --
bt :: (a -> Bool) -> (a -> [a]) -> a -> [a]
bt    esSol          suc          n
  | esSol n                           = [n]
  | otherwise                         = concatMap (bt esSol suc) (suc n)


-- Main function: solves a Futoshiki --
solve :: Futoshiki -> [Matrix Int]
solve f = map cells (bt esSol suc f)

-- Comprueba si no quedan ceros.
esSol :: Futoshiki -> Bool
esSol f = noHayCeros (concat (cells f))
  where
    noHayCeros [] = True
    noHayCeros (x:xs)
      | x == 0    = False
      | otherwise = noHayCeros xs

-- Genera hijos a partir de la primera casilla vacía.
suc :: Futoshiki -> [Futoshiki]
suc f = generaHijos f (buscarCero (cells f) 0)

-- Utiliza listas por comprensión para la generación.
-- Si buscarCero devolvió (-1, -1), la lista de hijos es vacía.
generaHijos :: Futoshiki -> (Int, Int) -> [Futoshiki]
generaHijos f (-1, -1) = []
generaHijos f (r, c) =
    [ Futoshiki (size f) (ponerValor (cells f) r c v) (hRel f) (vRel f)
    | v <- [1 .. size f]
    , valido f r c v
    ]


buscarCero :: Matrix Int -> Int -> (Int, Int)
buscarCero [] _ = (-1, -1)
buscarCero (fila:filas) r =
    let c = buscarEnFila fila 0
    in if c /= -1 then (r, c) else buscarCero filas (r + 1)

buscarEnFila :: [Int] -> Int -> Int
buscarEnFila [] _ = -1
buscarEnFila (x:xs) c
    | x == 0    = c
    | otherwise = buscarEnFila xs (c + 1)

-- Genera una nueva matriz con el valor "v" en la posición (r, c).
ponerValor :: Matrix Int -> Int -> Int -> Int -> Matrix Int
ponerValor matriz r c v =
    let fila = obtener matriz r
        nuevaFila = take c fila ++ [v] ++ drop (c + 1) fila
    in take r matriz ++ [nuevaFila] ++ drop (r + 1) matriz

-- Extrae un elemento de una lista buscando por índice.
obtener :: [a] -> Int -> a
obtener (x:xs) 0 = x
obtener (x:xs) n = obtener xs (n - 1)

obtenerColumna :: Matrix Int -> Int -> [Int]
obtenerColumna matriz c = [ obtener fila c | fila <- matriz ]

valido :: Futoshiki -> Int -> Int -> Int -> Bool
valido f r c v =
    noContiene (obtener (cells f) r) v &&
    noContiene (obtenerColumna (cells f) c) v &&
    validoH f r c v &&
    validoV f r c v

-- Detiene la recursión inmediatamente al encontrar el número "v".
noContiene :: [Int] -> Int -> Bool
noContiene [] _ = True
noContiene (x:xs) v
    | x == v    = False
    | otherwise = noContiene xs v

-- Las comprobaciones de los bordes (c == 0) mediante el operador ||
validoH :: Futoshiki -> Int -> Int -> Int -> Bool
validoH f r c v =
    (c == 0 || comprobarIzq f r c v) &&
    (c == size f - 1 || comprobarDer f r c v)

comprobarIzq :: Futoshiki -> Int -> Int -> Int -> Bool
comprobarIzq f r c v =
    let vIzq = obtener (obtener (cells f) r) (c - 1)
        rel = obtener (obtener (hRel f) r) (c - 1)
    in vIzq == 0 || cumpleRelacion vIzq rel v

comprobarDer :: Futoshiki -> Int -> Int -> Int -> Bool
comprobarDer f r c v =
    let vDer = obtener (obtener (cells f) r) (c + 1)
        rel = obtener (obtener (hRel f) r) c
    in vDer == 0 || cumpleRelacion v rel vDer

-- Igual mecanismo para las adyacencias verticales.
validoV :: Futoshiki -> Int -> Int -> Int -> Bool
validoV f r c v =
    (r == 0 || comprobarArriba f r c v) &&
    (r == size f - 1 || comprobarAbajo f r c v)

comprobarArriba :: Futoshiki -> Int -> Int -> Int -> Bool
comprobarArriba f r c v =
    let vArr = obtener (obtener (cells f) (r - 1)) c
        rel = obtener (obtener (vRel f) (r - 1)) c
    in vArr == 0 || cumpleRelacion vArr rel v

comprobarAbajo :: Futoshiki -> Int -> Int -> Int -> Bool
comprobarAbajo f r c v =
    let vAba = obtener (obtener (cells f) (r + 1)) c
        rel = obtener (obtener (vRel f) r) c
    in vAba == 0 || cumpleRelacion v rel vAba

cumpleRelacion :: Int -> Relation -> Int -> Bool
cumpleRelacion _ Ind _ = True
cumpleRelacion v1 RGT v2 = v1 > v2
cumpleRelacion v1 RLT v2 = v1 < v2