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
  | otherwise                         = concat (map (bt esSol suc) (suc n))


--------------------------------------------------------
----- -------IMPLEMENTACIÓN FUNCION SUC ----------------
suc :: Futoshiki -> [Futoshiki]
suc f = case encontrarCero (cells f) of
  []     -> []
  ((r, c):_) -> filter esValidoParcial
                   [ f { cells = replaceMatrix (cells f) r c v } | v <- [1 .. size f] ]

-- Encuentra la primera casilla vacía (0)
encontrarCero :: Matrix Int -> [(Int, Int)]
encontrarCero mat = [ (r, c) | (r, row) <- zip [0..] mat, (c, val) <- zip [0..] row, val == 0 ]

-- Reemplaza el valor en una posición de la matriz
replaceMatrix :: Matrix Int -> Int -> Int -> Int -> Matrix Int
replaceMatrix mat r c val =
  let beforeR = take r mat
      rowAfterR = drop r mat
  in case rowAfterR of
       [] -> mat
       (row:afterR) ->
          let beforeC = take c row
              afterC = drop (c + 1) row
              newRow = beforeC ++ [val] ++ afterC
          in beforeR ++ [newRow] ++ afterR

-- Comprueba si un estado parcial es válido (ignora los 0s)
esValidoParcial :: Futoshiki -> Bool
esValidoParcial f =
  let filas = cells f
      columnas = transpose filas
      relVert = transpose (vRel f)
  in all noDups filas &&
     all noDups columnas &&
     all (uncurry checkRelFilaParcial) (zip filas (hRel f)) &&
     all (uncurry checkRelFilaParcial) (zip columnas relVert)

-- Función auxiliar que se detiene al primer hallazgo (cortocircuito)
pertenece :: Int -> [Int] -> Bool
pertenece _ [] = False
pertenece e (y:ys)
  | e == y    = True
  | otherwise = pertenece e ys

noDups :: [Int] -> Bool
noDups [] = True
noDups (x:xs)
  | x == 0    = noDups xs
  | pertenece x xs = False
  | otherwise = noDups xs

-- Funciones para comprobar relaciones en estados parciales (ignora si hay un 0)
cumpleRelParcial :: (Int, Int) -> Relation -> Bool
cumpleRelParcial (0, _) _ = True
cumpleRelParcial (_, 0) _ = True
cumpleRelParcial _ Ind = True
cumpleRelParcial (v1, v2) RLT = v1 < v2
cumpleRelParcial (v1, v2) RGT = v1 > v2

checkRelFilaParcial :: [Int] -> [Relation] -> Bool
checkRelFilaParcial nums rels =
  let pares = zip nums (tail nums)
  in all (uncurry cumpleRelParcial) (zip pares rels)


-------------------------------------------------------
------------IMPLEMENTACIÓN FUNCION ESSOL --------------

esSol :: Futoshiki -> Bool
esSol f =
  let filas = cells f
      columnas = transpose filas
      relVert  = transpose (vRel f)
  in noHayCeros (concat filas) &&
     all (uncurry checkRelFila) (zip filas (hRel f)) &&
     all (uncurry checkRelFila) (zip columnas relVert)

-- Comprueba si hay algún 0 en una lista plana
noHayCeros :: [Int] -> Bool
noHayCeros [] = True
noHayCeros (x:xs)
  | x == 0    = False -- Cortocircuito: si vemos un cero, fallamos al instante
  | otherwise = noHayCeros xs

-- Comprueba que una lista de enteros contiene exactamente los números del 1 a n, sin repetir.
-- Esto también asegura que no hay ceros.
listaValida :: Int -> [Int] -> Bool
listaValida n xs = todosPertenecen [1..n] xs

-- Comprueba si una relación se cumple entre un par de números.
cumpleRel :: (Int, Int) -> Relation -> Bool
cumpleRel _ Ind = True
cumpleRel (v1, v2) RLT = v1 < v2
cumpleRel (v1, v2) RGT = v1 > v2

-- Comprueba las relaciones para una "fila" (puede ser una fila o una columna de celdas).
checkRelFila :: [Int] -> [Relation] -> Bool
checkRelFila (x:y:xs) (r:rs) = cumpleRel (x, y) r && checkRelFila (y:xs) rs
checkRelFila _ _ = True

todosPertenecen :: [Int] -> [Int] -> Bool
todosPertenecen [] _ = True
todosPertenecen (x:xs) listaDestino = pertenece x listaDestino && todosPertenecen xs listaDestino

-- Main function: solves a Futoshiki --
solve :: Futoshiki -> [Matrix Int]
solve f = map cells (bt esSol suc f)