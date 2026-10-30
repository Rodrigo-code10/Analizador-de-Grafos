module Dot
  ( generarDot )
   where

import Types

-- Lista de colores (Se repite si hay más)
colores :: [String]
colores = ["blue", "red", "darkgreen", "orange", "purple", "brown"]

-- Aristas dentro de una componente. 
-- Usamos i < j para no repetir, ya que arista 0--1 == 1-0
aristasDe :: Matriz Int -> [Nodo] -> [(Nodo, Nodo)]
aristasDe a c = [ (i, j) | i <- c, j <- c, i < j, a !! i !! j == 1 ]

-- Texto de un recuadro para componente
bloque :: Matriz Int -> Int -> String -> [Nodo] -> [String]
bloque a k color c =
  [ "  subgraph cluster_" ++ show k ++ " {"
  , "    label=\"Componente " ++ show (k + 1) ++ "\";"
  , "    color=" ++ color ++ ";"
  , "    node [color=" ++ color ++ "];"
  , "    edge [color=" ++ color ++ "];"
  ]
  ++ map (\n -> "    " ++ show n ++ ";") c
  ++ map (\(i, j) -> "    " ++ show i ++ " -- " ++ show j ++ ";") (aristasDe a c)
  ++ [ "  }" ]

-- Función principal, pone el texto en una string
generarDot :: Matriz Int -> [[Nodo]] -> String
generarDot a cs = unlines $
  [ "graph G {"
  , "  node [shape=circle];"
  ]
  ++ concat (zipWith3 (bloque a) [0 ..] (cycle colores) cs)
  ++ [ "}" ]