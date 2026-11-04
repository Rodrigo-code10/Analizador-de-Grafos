module Dot
  ( generarDot, generarDotEstado )
   where

import Types
import Text.Printf (printf)

-- Lista de colores (Se repite si hay más)
colores :: [String]
colores = ["blue", "red", "darkgreen", "orange", "purple", "brown"]

-- Aristas dentro de una componente 
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

-- Color según el valor de 0-100: azul = bajo, rojo = alto.
colorNodo :: Double -> String
colorNodo x = printf "#%02x50%02x" r b
  where
    t = max 0 (min 1 (x / 100))
    r = round (255 * t) :: Int
    b = round (255 * (1 - t)) :: Int

-- Un recuadro por componente
bloqueEstado :: Double -> Matriz Int -> [Double] -> Int -> [Nodo] -> [String]
bloqueEstado tol a x k c =
  [ "  subgraph cluster_" ++ show k ++ " {"
  , "    label=\"Componente " ++ show (k + 1) ++ etiqueta ++ "\";"
  , "    color=" ++ borde ++ ";"
  , "    penwidth=3;"
  ]
  ++ map nodo c
  ++ map (\(i, j) -> "    " ++ show i ++ " -- " ++ show j ++ ";") (aristasDe a c)
  ++ [ "  }" ]
  where
    vs       = map (x !!) c
    ok       = maximum vs - minimum vs < tol
-- Bode verde si ya quedo
    borde    = if ok then "green" else "gray"
    etiqueta = if ok then " (armonizada)" else ""
    nodo n   = "    " ++ show n ++ " [fillcolor=\"" ++ colorNodo (x !! n)
               ++ "\", label=\"" ++ show n ++ "\\n" ++ printf "%.1f" (x !! n) ++ "\"];"

-- DOT completo con nodos coloreados
generarDotEstado :: Double -> Matriz Int -> [[Nodo]] -> [Double] -> String
generarDotEstado tol a cs x = unlines $
  [ "graph G {"
  , "  node [shape=circle, style=filled, fontcolor=white];"
  ]
  ++ concat (zipWith (bloqueEstado tol a x) [0 ..] cs)
  ++ [ "}" ]