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
suc f = case findEmpty (cells f) of
  Nothing     -> []
  Just (r, c) -> filter esValidoParcial
                   [ f { cells = replaceMatrix (cells f) (r, c) v } | v <- [1 .. size f] ]

-- Encuentra la primera casilla vacía (0)
findEmpty :: Matrix Int -> Maybe (Int, Int)
findEmpty mat = case [ (r, c) | (r, row) <- zip [0..] mat, (c, val) <- zip [0..] row, val == 0 ] of
  []        -> Nothing
  (coord:_) -> Just coord

-- Reemplaza el valor en una posición de la matriz
replaceMatrix :: Matrix Int -> (Int, Int) -> Int -> Matrix Int
replaceMatrix mat (r, c) val =
  let (beforeR, row:afterR) = splitAt r mat
      (beforeC, _:afterC)   = splitAt c row
      newRow = beforeC ++ [val] ++ afterC
  in beforeR ++ [newRow] ++ afterR

-- Comprueba si un estado parcial es válido (ignora los 0s)
esValidoParcial :: Futoshiki -> Bool
esValidoParcial f =
  all noDups (cells f) &&
  all noDups (transpose (cells f)) &&
  all (uncurry checkRelFilaParcial) (zip (cells f) (hRel f)) &&
  all (uncurry checkRelFilaParcial) (zip (transpose (cells f)) (transpose (vRel f)))

noDups :: [Int] -> Bool
noDups [] = True
noDups (x:xs)
  | x == 0    = noDups xs
  | otherwise = not (x `elem` xs) && noDups xs

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
  filasValidas f &&
  columnasValidas f &&
  comprobarRelacionesHorizontales f &&
  comprobarRelacionesVerticales f

-- Comprueba que una lista de enteros contiene exactamente los números del 1 a n, sin repetir.
-- Esto también asegura que no hay ceros.
listaValida :: Int -> [Int] -> Bool
listaValida n xs = sort xs == [1..n]

-- Comprueba que todas las filas de la matriz son válidas.
filasValidas :: Futoshiki -> Bool
filasValidas f = all (listaValida (size f)) (cells f)

-- Comprueba que todas las columnas de la matriz son válidas, usando transposición.
columnasValidas :: Futoshiki -> Bool
columnasValidas f = all (listaValida (size f)) (transpose (cells f))

-- Comprueba si una relación se cumple entre un par de números.
cumpleRel :: (Int, Int) -> Relation -> Bool
cumpleRel _ Ind = True
cumpleRel (v1, v2) RLT = v1 < v2
cumpleRel (v1, v2) RGT = v1 > v2

-- Comprueba las relaciones para una "fila" (puede ser una fila o una columna de celdas).
checkRelFila :: [Int] -> [Relation] -> Bool
checkRelFila nums rels =
  let pares = zip nums (tail nums)
  in all (uncurry cumpleRel) (zip pares rels)

-- Comprueba todas las relaciones horizontales.
comprobarRelacionesHorizontales :: Futoshiki -> Bool
comprobarRelacionesHorizontales f = all (uncurry checkRelFila) (zip (cells f) (hRel f))

-- Comprueba todas las relaciones verticales, reutilizando la lógica mediante transposición.
comprobarRelacionesVerticales :: Futoshiki -> Bool
comprobarRelacionesVerticales f = all (uncurry checkRelFila) (zip (transpose (cells f)) (transpose (vRel f)))


-- Main function: solves a Futoshiki --
solve :: Futoshiki -> [Matrix Int]
solve f = map cells (bt esSol suc f)