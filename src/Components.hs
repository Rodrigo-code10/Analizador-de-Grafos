module Components
  ( vecinos, componentes )
   where

import Data.List (sort)
import Types

-- Vecinos de un nodo, es donde las posiciones donde su fila tiene un 1
vecinos :: Matriz Int -> Nodo -> [Nodo]
vecinos a i = [ j | (j, x) <- zip [0 ..] (a !! i), x == 1 ]

-- Explora desde una pila de nodos pendientes, acumulando los visitados
explorar :: Matriz Int -> [Nodo] -> [Nodo] -> [Nodo]
explorar _ visitados [] = visitados 
explorar a visitados (x : pila)
  | x `elem` visitados = explorar a visitados pila
  | otherwise          = explorar a (x : visitados) (vecinos a x ++ pila)

-- Toma un nodo sin visitar, explora, y repite con el resto
componentes :: Matriz Int -> [[Nodo]]
componentes a = ir [0 .. length a - 1] []
  where
    ir [] _ = []
    ir (n : ns) vistos
      | n `elem` vistos = ir ns vistos
      | otherwise       =
          let comp = sort (explorar a [] [n])
          in  comp : ir ns (vistos ++ comp)