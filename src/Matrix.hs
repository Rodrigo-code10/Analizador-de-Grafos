module Matrix
  ( esCuadrada, esSimetrica
  , diagonal, diagonalEnCero
  , traza, grados, validar )
   where

import Data.List (transpose)
import Types

-- Cuadrada: La matriz es nxn
esCuadrada :: Matriz a -> Bool
esCuadrada m = all ((== n) . length) m
  where n = length m

-- Simétrica: la matriz es igual a su transpuesta (filas <--> columnas)
esSimetrica :: Eq a => Matriz a -> Bool
esSimetrica m = m == transpose m

-- toma el elemento [i][i]
diagonal :: Matriz a -> [a]
diagonal m = zipWith (!!) m [0 ..]

diagonalEnCero :: (Eq a, Num a) => Matriz a -> Bool
diagonalEnCero = all (== 0) . diagonal

-- Suma valores de la diagonal
traza :: Num a => Matriz a -> a
traza = sum . diagonal

-- Obtiene el grado de cada nodo con la suma de la fila
grados :: Matriz Int -> [Int]
grados = map sum

-- Valida que sea una matriz de adyacencia correcta.
-- Either: 
--  Left = error, muestra mensaje.
--  Right = Esta bien, devuelve la matriz.
validar :: Matriz Int -> Either String (Matriz Int)
validar m
  | not (esCuadrada m)             = Left "La matriz no es cuadrada."
  | not (all (all (`elem` [0, 1])) m) = Left "Solo se permiten 0 y 1."
  | not (esSimetrica m)            = Left "La matriz no es simétrica."
  | not (diagonalEnCero m)         = Left "La diagonal debe ser 0."
  | otherwise                      = Right m
