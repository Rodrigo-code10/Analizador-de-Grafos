module Laplacian
  ( matrizGrados, laplaciana, filasSuman0 ) 
  where

import Matrix
import Types

-- Matriz D: Matriz en ceros con los grados en la diagonal
matrizGrados :: Matriz Int -> Matriz Int
matrizGrados a =
  [ [ if i == j then g else 0 | j <- [0 .. n - 1] ]
  | (i, g) <- zip [0 ..] (grados a)
  ]
  where n = length a

-- L = D - A, restando elemento por elemento
laplaciana :: Matriz Int -> Matriz Int
laplaciana a = zipWith (zipWith (-)) (matrizGrados a) a

-- Comprobando que en una Laplaciano la suma de cada fila sea 0
filasSuman0 :: Matriz Int -> Bool
filasSuman0 = all ((== 0) . sum)