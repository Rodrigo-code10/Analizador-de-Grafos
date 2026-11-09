module Spectrum
  ( autovalores) 
  where

import Data.List (sort, transpose)
import Matrix (diagonal)
import Types

-- Producto de matrices es en si fila por columna
producto :: Num a => Matriz a -> Matriz a -> Matriz a
producto a b = [ [ sum (zipWith (*) fila col) | col <- transpose b ] | fila <- a ]

-- Mayor elemento fuera de la diagonal: (valor absoluto, fila, columna)
mayorFuera :: Matriz Double -> (Double, Int, Int)
mayorFuera m = maximum ((0, 0, 0) :
  [ (abs x, i, j) | (i, fila) <- zip [0 ..] m, (j, x) <- zip [0 ..] fila, i < j ])

-- Una rotación de Jacobi: anula el mayor elemento fuera de la diagonal
-- Devuelve  Jt * M * J  
rotar :: Matriz Double -> Matriz Double
rotar m
  | apq == 0  = m
  | otherwise = producto (transpose j) (producto m j)
  where
    n         = length m
    (_, p, q) = mayorFuera m
    apq       = m !! p !! q
    tau       = (m !! q !! q - m !! p !! p) / (2 * apq)
    t         = (if tau >= 0 then 1 else -1) / (abs tau + sqrt (1 + tau * tau))
    c         = 1 / sqrt (1 + t * t)
    s         = t * c
    j         = [ [ entrada i k | k <- [0 .. n - 1] ] | i <- [0 .. n - 1] ]
    entrada i k
      | i == p && k == p = c
      | i == q && k == q = c
      | i == p && k == q = s
      | i == q && k == p = -s
      | i == k           = 1
      | otherwise        = 0

-- Jacobi por recursión: si la matriz ya es (casi) diagonal se devuelve tal cual;
-- si no, se rota y se repite con la matriz nueva
jacobi :: Int -> Matriz Double -> Matriz Double
jacobi 0 m = m
jacobi k m
  | convergida m = m
  | otherwise    = jacobi (k - 1) (rotar m)

convergida :: Matriz Double -> Bool
convergida x = let (v, _, _) = mayorFuera x in v < 1e-10

-- La diagonal de la matriz ya diagonalizada.
autovalores :: Matriz Int -> [Double]
autovalores l = sort (map limpia (diagonal (jacobi (50 * n * n) m)))
  where
    m        = map (map fromIntegral) l
    n        = length m
    limpia x = if abs x < 1e-9 then 0 else x