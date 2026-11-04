module Consenso
  ( Estado, Fijos, epsilonSeguro, paso, pasoFijo, 
  rondas, desarmonia, desarmoniaLibre, 
  armonizado, armonizadaComp, historial, promedios) 
  where

import Types
import Matrix (grados)

-- Un estado es el puntaje de cada nodo: posición i = puntaje del nodo i
type Estado = [Double]

-- Producto L · x: para cada fila de L, multiplica por los puntajes y suma
-- Mide cuánto se diferencia cada nodo de sus vecinos
multiplicar :: Matriz Int -> Estado -> Estado
multiplicar l x = [ sum (zipWith (*) (map fromIntegral fila) x) | fila <- l ]

-- El tamaño del paso debe der menor que 1 / (grado máximo) para no desestabilizarse
epsilonSeguro :: Matriz Int -> Double
epsilonSeguro a = 1 / (fromIntegral (maximum (0 : grados a)) + 1)

-- una ronda: nuevo = x - eps * (L · x)
paso :: Double -> Matriz Int -> Estado -> Estado
paso eps l x = zipWith (\xi lxi -> xi - eps * lxi) x (multiplicar l x)

-- Lista infinita de rondas: x0, paso x0, 
rondas :: Double -> Matriz Int -> Estado -> [Estado]
rondas eps l = iterate (paso eps l)

-- Qué tan lejos está del consenso, tomando el mayor valor absoluto de L · x
desarmonia :: Matriz Int -> Estado -> Double
desarmonia l x = maximum (0 : map abs (multiplicar l x))

-- Consenso
armonizado :: Double -> Matriz Int -> Estado -> Bool
armonizado tol l x = desarmonia l x < tol

-- Rondas hasta converger 
historial :: Double -> Double -> Int -> Matriz Int -> Estado -> [Estado]
historial eps tol maxR l x0 =
  let (antes, despues) = span (not . armonizado tol l) (take maxR (rondas eps l x0))
  in  antes ++ take 1 despues

-- Valor al que debe llegar cada componente, es el promedio de sus puntajes iniciales
promedios :: [[Nodo]] -> Estado -> [([Nodo], Double)]
promedios cs x =
  [ (c, sum vs / fromIntegral (length vs))
  | c <- cs
  , let vs = map (x !!) c
  ]

-- Un valor Bool por nodo: True = el nodo está fijo.
type Fijos = [Bool]

-- Igual que `paso`, pero los nodos fijos conservan su valor.
pasoFijo :: Double -> Matriz Int -> Fijos -> Estado -> Estado
pasoFijo eps l fs x = zipWith3 actualizar fs x (multiplicar l x)
  where
    actualizar fijo xi lxi
      | fijo      = xi
      | otherwise = xi - eps * lxi

-- Desarmonía ignorando los nodos fijos (ellos no tienen que estar en equilibrio
desarmoniaLibre :: Matriz Int -> Fijos -> Estado -> Double
desarmoniaLibre l fs x =
  maximum (0 : [ abs v | (f, v) <- zip fs (multiplicar l x), not f ])

-- Una componente está armonizada? Todos sus nodos libres tienen |L·x| pequeño
armonizadaComp :: Double -> Matriz Int -> Fijos -> Estado -> [Nodo] -> Bool
armonizadaComp tol l fs x = all ok
  where
    lx   = multiplicar l x
    ok i = fs !! i || abs (lx !! i) < tol