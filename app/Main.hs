module Main (main) where

import Matrix
import Text.Read (readMaybe)
import Types
import Laplacian
import Components
import Rank
import Dot

-- Convierte el texto, en una matriz de enteros.
parsear :: String -> Either String (Matriz Int)
parsear txt = case lines txt of
  [] -> Left "El archivo está vacío."
  (primera : resto) -> do
    n <- maybe (Left "La primera línea debe ser un número.") Right (readMaybe primera)
    filas <- traverse leerFila (take n resto)

    if length filas /= n
      then Left "Faltan filas en el archivo."
      else Right filas
  where
    leerFila l = maybe (Left ("Fila inválida: " ++ l)) Right (traverse readMaybe (words l))


main :: IO ()
main = do
  texto <- readFile "data/grafo.txt"
  case parsear texto >>= validar of
    Left err -> putStrLn ("Error: " ++ err)
    Right a  -> do
      let l = laplaciana a
      putStrLn ("Nodos:    " ++ show (length a))
      putStrLn ("Grados:   " ++ show (grados a))
      putStrLn ("Aristas:  " ++ show (sum (grados a) `div` 2))
      
      putStrLn "Laplaciana L:"
      mapM_ (putStrLn . unwords . map show) l
      putStrLn ("Traza de L: " ++ show (traza l))
      putStrLn ("Filas suman 0: " ++ show (filasSuman0 l))

      let cs = componentes a
      putStrLn ("Componentes conexas: " ++ show (length cs))
      mapM_ (\(k, c) -> putStrLn ("  Componente " ++ show k ++ ": " ++ show c))
            (zip [1 :: Int ..] cs)

      let r = rangoDe l
      putStrLn ("Rango de L: " ++ show r)
      putStrLn ("n - rango:  " ++ show (length a - r))
      putStrLn ("Verificación: " ++ show (length a - r == length cs))

      writeFile "output/grafo.dot" (generarDot a cs)
      putStrLn "Archivo generado: output/grafo.dot"