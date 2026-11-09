module Main (main) where

import Components
import Consenso
import Dot
import Laplacian
import Matrix
import Rank
import Spectrum
import System.Environment (getArgs)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Types

tolerancia :: Double
tolerancia = 0.01

leerLista :: Read a => String -> String -> Either String [a]
leerLista msg l = maybe (Left (msg ++ ": " ++ l)) Right (traverse readMaybe (words l))

parsearEntrada :: String -> Either String (Matriz Int, [Double], Fijos)
parsearEntrada txt = case lines txt of
  []          -> Left "Entrada vacía."
  (p : resto) -> do
    n <- maybe (Left "La primera línea debe ser un número.") Right (readMaybe p)
    let (filasTxt, sobran) = splitAt n resto
    filas <- traverse (leerLista "Fila inválida") filasTxt
    if length filas /= n then Left "Faltan filas." else Right ()
    puntajes <- case sobran of
      (s : _) -> leerLista "Puntajes inválidos" s
      []      -> Left "Faltan los puntajes."
    if length puntajes /= n then Left "Debe haber un puntaje por nodo." else Right ()
    fijosInt <- case drop 1 sobran of
      (f : _) -> leerLista "Fijos inválidos" f
      []      -> Right (replicate n (0 :: Int))
    if length fijosInt /= n then Left "Debe haber un indicador de fijo por nodo." else Right ()
    Right (filas, puntajes, map (== 1) fijosInt)

informe :: Matriz Int -> Fijos -> Estado -> String
informe a fs x = unlines $
  [ "nodos " ++ show (length a)
  , "aristas " ++ show (sum (grados a) `div` 2)
  , "traza " ++ show (traza l)
  , "rango " ++ show r
  , "componentes " ++ show (length cs)
  , "verificacion " ++ show (length a - r == length cs)
  , "grados " ++ unwords (map show (grados a))
  , "desarmonia " ++ printf "%.4f" (desarmoniaLibre l fs x)
  , "armonizado " ++ show (desarmoniaLibre l fs x < tolerancia)
  , "armcomp " ++ unwords (map (\b -> if b then "1" else "0") armC)
  , "estado " ++ unwords (map (printf "%.6f") x)
  ]
  ++ map (("comp " ++) . unwords . map show) cs
  ++ [ "---DOT---", generarDotEstado tolerancia a cs x ]
  where
    l    = laplaciana a
    cs   = componentes a
    r    = rangoDe l
    armC = map (armonizadaComp tolerancia l fs x) cs

-- Informe espectral: matrices, autovalores y comprobaciones.
informeEspectro :: Matriz Int -> String
informeEspectro a = unlines $
  [ "nodos " ++ show (length a)
  , "aristas " ++ show (sum (grados a) `div` 2)
  , "traza " ++ show (traza l)
  , "rango " ++ show r
  , "componentes " ++ show (length cs)
  , "verificacion " ++ show (length a - r == length cs)
  ]
  ++ map (("A " ++) . fila) a
  ++ map (("D " ++) . fila) (matrizGrados a)
  ++ map (("L " ++) . fila) l
  ++ [ "autovalores " ++ unwords (map (printf "%.6f") ev)
     , "suma " ++ printf "%.6f" (sum ev)
     , "ceros " ++ show (length (filter (< 1e-6) ev))
     , "filasuman0 " ++ show (filasSuman0 l)
     ]
  ++ map (("comp " ++) . unwords . map show) cs
  where
    l    = laplaciana a
    cs   = componentes a
    r    = rangoDe l
    ev   = autovalores l
    fila = unwords . map show

main :: IO ()
main = do
  args  <- getArgs
  texto <- getContents
  let entrada = do
        (a0, x, fs) <- parsearEntrada texto
        a <- validar a0
        Right (a, x, fs)
  case (args, entrada) of
    (_, Left err)                    -> putStrLn ("ERROR " ++ err)
    (["analizar"], Right (a, x, fs)) -> putStr (informe a fs x)
    (["paso"], Right (a, x, fs))     ->
      putStr (informe a fs (pasoFijo (epsilonSeguro a) (laplaciana a) fs x))
    (["dot"], Right (a, x, _))       ->
      putStr (generarDotEstado tolerancia a (componentes a) x)
    (["espectro"], Right (a, _, _))  -> putStr (informeEspectro a)
    _ -> putStrLn "ERROR Comando desconocido. Usa: analizar | paso | dot | espectro"