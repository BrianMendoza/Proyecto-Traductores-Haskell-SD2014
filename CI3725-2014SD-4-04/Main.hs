-- CI 3725 Septiembre-Diciembre 2014
-- Proyecto, Entrega 4
-- Integrantes:
--		Brian Mendoza 07-41206

module Main (main) where

import AST
import Contexto
import Control.DeepSeq
import Control.Monad
import Interprete
import Lexer
import Parser
import System.Environment
import System.Exit
import System.IO
import Text.Show.Pretty
import Tokens

usageError :: IO()
usageError = error $ "Usage: debe haber solo 1 argumento, el archivo a utilizar"

mostrarLex :: String -> IO()
mostrarLex s
  = do
    codigo <- readFile s
    let listaTokens = alexScanTokens codigo
    mapM_ printTok listaTokens -- tambien sirve for_ (alexScanTokens codigo) printTok
    if noHayError listaTokens
      then exitSuccess
      else exitFailure

analizarAST :: String -> IO()
analizarAST s
  = do
    codigo <- readFile s
    let listaTokens = alexScanTokens codigo
    if noHayError listaTokens
      then printAST listaTokens
      else printError listaTokens

analizarContexto :: String -> IO()
analizarContexto s
  = do
    codigo <- readFile s
    let listaTokens = alexScanTokens codigo
    if noHayError listaTokens
      then printContexto listaTokens
      else printError listaTokens

correrPrograma :: String -> IO()
correrPrograma s
  = do
    codigo <- readFile s
    let listaTokens = alexScanTokens codigo
    if noHayError listaTokens
      then interpretar listaTokens
      else printError listaTokens

interpretar :: [Token] -> IO()
interpretar l
  = do
    let programa = parseTrinity l
    let alcances = generarAlcances programa
    return $! rnf alcances
    run programa
    exitSuccess

printContexto :: [Token] -> IO()
printContexto = putStrLn . ppShow . generarAlcances . parseTrinity

printAST :: [Token] -> IO()
printAST = putStrLn . ppShow . parseTrinity
	

printError :: [Token] -> IO()
printError s = do
  mapM_ printTok $ listErrors s
  exitFailure

printTok :: Token -> IO()
printTok t
  = putStrLn
  $ "Linea " ++ show linea
  ++ ", columna " ++ show columna ++ ": "
  ++ token_name t 
  ++ (case t of
    TokenNumLit _ v -> ": " ++ show v
    TokenStrLit _ v -> ": " ++ v
    TokenIdent _ v -> ": " ++ v
    TokenError _ v -> ": " ++ v
    _ -> ""
  )
  where Posicion linea columna = token_posn t



main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  let numArgs = length args
  when (numArgs /= 1) usageError
  --when (numArgs == 1) (mostrarLex $ args !! 0)
  --when (numArgs == 1) (analizarAST $ args !! 0)
  --when (numArgs == 1) (analizarContexto $ args !! 0)
  when (numArgs == 1) (correrPrograma $ args !! 0)

