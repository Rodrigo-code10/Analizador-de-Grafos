module Rank
  ( rango, rangoDe )
   where

import Types

-- Rango por eliminación gaussiana, con numeros exactos, evita redondeos con decimales
rango :: Matriz Rational -> Int
rango []        = 0
rango ([] : _)  = 0
rango m =
  case span ((== 0) . head) m of
    -- Primera columna es toda ceros, por lo que la descartamos y seguimos
    (_, [])          -> rango (map tail m)
    -- Encontramos un pivote (p), lo usamos para limpiar las demás filas
    (ceros, p : resto) ->
      let otras      = ceros ++ resto
          reducir r  = let f = head r / head p
                       in  tail (zipWith (\pv rv -> rv - f * pv) p r)
      in  1 + rango (map reducir otras)

-- Recibe matriz (enteros) y convierte en Rational
rangoDe :: Matriz Int -> Int
rangoDe = rango . map (map fromIntegral)