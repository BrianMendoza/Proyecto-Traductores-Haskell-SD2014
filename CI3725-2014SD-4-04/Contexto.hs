-- CI 3725 Septiembre-Diciembre 2014
-- Proyecto, Entrega 4
-- Integrantes:
--    Brian Mendoza 07-41206

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Contexto where

import AST
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Control.Applicative ((<|>), (<$>), empty, pure)
import Control.DeepSeq.Generics
import Data.Maybe
import GHC.Generics (Generic)
import Tokens

data TipoDeAlcance =
  EnFuncion { retorna :: Tipo } |
  EnMain
  deriving (Eq, Show, Generic)

data Alcance =
  Alcance
  { simbolos :: Map.Map String Tipo
  , hijos :: [Alcance]
  , tipoDeAlcance :: TipoDeAlcance
  }
  deriving (Eq, Show, Generic)

instance NFData Alcance where rnf = genericRnf
instance NFData TipoDeAlcance where rnf = genericRnf

generarAlcances :: Program -> [Alcance]
generarAlcances p
  = if checkFunctions p
    then buildSymTable p
    else error $ "Mas de una funcion declarada con el mismo nombre"

buildSymTable :: Program -> [Alcance]
buildSymTable (Program funciones instrucciones)
  = map checkF funciones ++ [main]
  where
    checkF :: Funcion -> Alcance
    checkF (Funcion _ params retorna instrucciones)
      = alcance
      where 
        alcance 
          = Alcance
            { simbolos = fromListWithKey' justNewInsert (toTuples params [] [])
            , hijos = checkI [alcance] funciones =<< instrucciones
            , tipoDeAlcance = EnFuncion {..}
            }

    main
      = Alcance
        { simbolos = Map.empty
        , hijos = checkI [main] funciones =<< instrucciones
        , tipoDeAlcance = EnMain
        }

    checkI :: [Alcance] -> [Funcion] -> Instruccion -> [Alcance]
    checkI alcances@(Alcance {..} : _) funciones
      = checkI'
      where
        checkI' (Read token)
          = case definiciones of
            [] -> errorPos "Variable no declarada"
            _ -> case head definiciones of
                  TipoBoolean -> []
                  TipoNumber  -> []
                  _           -> errorPos "Error de tipo"

          where
            var = token_string token
            definiciones = findDef alcances var
            errorPos e
              = error $ e
              ++ " en línea " ++ show linea
              ++ ", columna " ++ show columna
              where
                Posicion linea columna = token_posn token

        checkI' (Use declaraciones instrucciones)
          = pure alcance
          where
            alcance
              = Alcance
                { simbolos = fromListWithKey' justNewInsert (toTuples declaraciones alcances funciones)
                , hijos = checkI alcances' funciones =<< instrucciones
                , ..
                }
              where
                alcances' = alcance : alcances

        checkI' (Asignacion lvalue expresion)
          = case definiciones of
                [] -> errorPos "Variable no declarada"
                _ -> case lvalue of
                     LValueIdentificador _ -> if head definiciones == tipoExp alcances funciones expresion
                                                then []
                                                else errorPos "Type clash en L-value"
                     LValueVector _ e      -> if head definiciones /= TipoBoolean && head definiciones /= TipoNumber 
                                                    && tipoExp alcances funciones e == TipoNumber && tipoExp alcances funciones expresion == TipoNumber
                                                then []
                                                else errorPos "Type clash en L-value"
                     LValueMatriz _ e1 e2  -> if head definiciones /= TipoBoolean && head definiciones /= TipoNumber 
                                                    && tipoExp alcances funciones e1 == TipoNumber && tipoExp alcances funciones e2 == TipoNumber
                                                    && tipoExp alcances funciones expresion == TipoNumber
                                                then []
                                                else errorPos "Type clash en L-value"
            where
              get_token (LValueIdentificador t) = t
              get_token (LValueVector t _) = t
              get_token (LValueMatriz t _ _) = t
              var = token_string $ get_token lvalue
              definiciones = findDef alcances var
              errorPos e
                = error $ e
                ++ " en línea " ++ show linea
                ++ ", columna " ++ show columna
                where
                  Posicion linea columna = token_posn $ get_token lvalue

        checkI' (IfThenElse expresion instruccionesThen instruccionesElse)
          = if tipoExp alcances funciones expresion == TipoBoolean
                then checkI' =<< (instruccionesThen <|> instruccionesElse)
                else error $ "Guarda de condicional if then else no es de tipo booleano"

        checkI' (IfThen expresion instruccionesThen)
          = if tipoExp alcances funciones expresion == TipoBoolean
                then checkI' =<< instruccionesThen
                else error $ "Guarda de condicional if then no es de tipo booleano"

        checkI' (Print impresiones)
          = checkImpresion =<< impresiones
          where
            checkImpresion :: Impresion -> [Alcance]
            checkImpresion i
              = case i of
                ImprimirExpresion e -> tipoExp alcances funciones e `seq` []
                ImprimirString _    -> []

        checkI' (While expresion instrucciones)
          = if tipoExp alcances funciones expresion == TipoBoolean
                then checkI' =<< instrucciones
                else error $ "Guarda de iteracion while no es de tipo booleano"

        checkI' (For identificador matriz instrucciones)
          = pure
          $! case tipoExp alcances funciones matriz of
                TipoMatriz _ _ -> alcance
                _ -> error $ "Rango del for debe ser de tipo matricial"
          where
            alcance
              = Alcance
                { simbolos = Map.singleton (token_string identificador) TipoNumber
                , hijos = checkI alcances' funciones =<< instrucciones
                , ..
                }
              where
                alcances' = alcance : alcances

        checkI' (Return expresion)
          = case tipoDeAlcance of
            EnFuncion {..}
              -> if retorna == tipoExp alcances funciones expresion
                 then []
                 else error $ "En instruccion return se esperaba una expresion de tipo "
                              ++ show retorna
            _ -> error $ "Solo se puede usar return en una funcion"

        checkI' (InstruccionExpresion expresion)
          = tipoExp alcances funciones expresion `seq` []


findDef :: [Alcance] -> String -> [Tipo]
findDef alcances var = catMaybes $ List.map (Map.lookup var) $ List.map simbolos alcances

toTuples :: ToTuple a => [a] -> [Alcance] -> [Funcion] -> [(String, Tipo)]
toTuples l alcances funciones = fmap (\ a -> toTuple a alcances funciones) l 

-- toTuple tiene que funcionar con *varios* tipos distintos y necesita un código distinto para cada tipo con el cual funciona:
class ToTuple a where
  toTuple :: a -> [Alcance] -> [Funcion] -> (String, Tipo)

instance ToTuple Parametro where
  toTuple (Parametro tipo token) _ _
    = case tipo of
        TipoMatriz f c -> if f > 0 && c > 0 && esEntero f && esEntero c
                            then (token_string token, tipo)
                            else errorPos "Matriz declarada con numero no entero de filas o columnas"
        _              -> (token_string token, tipo)
      where
        esEntero n 
          = n - (fromIntegral $ floor n) == 0
        errorPos e
          = error $ e
          ++ " en línea " ++ show linea
          ++ ", columna " ++ show columna
          where
            Posicion linea columna = token_posn token
        

instance ToTuple Declaracion where
  toTuple d alcances funciones
    = case d of
      DeclaracionNoInicializada tipo token   
        -> case tipo of
            TipoMatriz f c -> if f > 0 && c > 0 && esEntero f && esEntero c
                                then (token_string token, tipo)
                                else errorPos "Matriz declarada con numero no entero de filas o columnas"
            _              -> (token_string token, tipo)

          where
            esEntero n 
              = n - (fromIntegral $ floor n) == 0
            errorPos e
              = error $ e
              ++ " en línea " ++ show linea
              ++ ", columna " ++ show columna
              where
                Posicion linea columna = token_posn token

      DeclaracionInicializada   tipo token expresion
        -> case tipo of
            TipoMatriz f c -> if f > 0 && c > 0 && esEntero f && esEntero c
                                then if tipoExp alcances funciones expresion == tipo
                                        then (token_string token, tipo)
                                        else errorPos "Type clash"
                                else errorPos "Matriz declarada con numero no entero de filas o columnas"
            _              -> if tipoExp alcances funciones expresion == tipo
                                then (token_string token, tipo)
                                else errorPos "Type clash"

          where
            esEntero n 
              = n - (fromIntegral $ floor n) == 0
            errorPos e
              = error $ e
              ++ " en línea " ++ show linea
              ++ ", columna " ++ show columna
              where
                Posicion linea columna = token_posn token

justNewInsert ::  Show k => k -> a -> a -> a
justNewInsert key _ _ = error $ "Mas de una declaracion de " ++ show key ++ " en el mismo alcance."

fromListWithKey' :: Ord k => (k -> a -> a -> a) -> [(k,a)] -> Map.Map k a 
fromListWithKey' f xs 
  = foldlStrict ins Map.empty xs
  where
    ins t (k,x) = Map.insertWithKey' f k x t

foldlStrict :: (a -> b -> a) -> a -> [b] -> a
foldlStrict f z xs
  = case xs of
      []     -> z
      (x:xx) -> let z' = f z x in seq z' (foldlStrict f z' xx)

tipoExp :: [Alcance] -> [Funcion] -> Expresion -> Tipo
tipoExp alcances funciones
    = tipoExp'
    where
        tipoExp' (Suma e1 e2)
            = if tipoExp' e1 == tipoExp' e2 && tipoExp' e1 /= TipoBoolean 
                then tipoExp' e1
                else typeClash "+" e1 e2

        tipoExp' (Resta e1 e2)
            = if tipoExp' e1 == tipoExp' e2 && tipoExp' e1 /= TipoBoolean 
                then tipoExp' e1
                else typeClash "-" e1 e2

        tipoExp' (Multiplicacion e1 e2)
            = case tipoExp' e1 of
               TipoNumber       -> if tipoExp' e2 == TipoNumber
                                      then TipoNumber
                                      else typeClash "*" e1 e2
               TipoMatriz f1 c1 -> case tipoExp' e2 of 
                                    TipoMatriz f2 c2 -> if c1 == f2
                                                          then (TipoMatriz f1 c2)
                                                          else error $ "Para multiplicacion de matrices debe cumplirse (MxN)*(NxP)"
                                    _                -> typeClash "*" e1 e2
               _                -> typeClash "*" e1 e2

        tipoExp' (DivisionExacta e1 e2)
            = if tipoExp' e1 == TipoNumber && tipoExp' e2 == TipoNumber
                then TipoNumber
                else typeClash "/" e1 e2

        tipoExp' (RestoExacto e1 e2)
            = if tipoExp' e1 == TipoNumber && tipoExp' e2 == TipoNumber
                then TipoNumber
                else typeClash "%" e1 e2

        tipoExp' (DivisionEntera e1 e2)
            = if tipoExp' e1 == TipoNumber && tipoExp' e2 == TipoNumber
                then TipoNumber
                else typeClash "div" e1 e2

        tipoExp' (RestoEntero e1 e2)
            = if tipoExp' e1 == TipoNumber && tipoExp' e2 == TipoNumber
                then TipoNumber
                else typeClash "mod" e1 e2

        tipoExp' (SumaCruzada e1 e2)
            = case (tipoExp' e1, tipoExp' e2) of
              (TipoBoolean, _)         -> typeClash ".+." e1 e2
              (_, TipoBoolean)         -> typeClash ".+." e1 e2
              (TipoNumber, TipoNumber) -> typeClash ".+." e1 e2
              (TipoNumber, _)          -> tipoExp' e2
              (_, TipoNumber)          -> tipoExp' e1
              _                        -> typeClash ".+." e1 e2

        tipoExp' (RestaCruzada e1 e2)
            = case (tipoExp' e1, tipoExp' e2) of
              (TipoBoolean, _)         -> typeClash ".-." e1 e2
              (_, TipoBoolean)         -> typeClash ".-." e1 e2
              (TipoNumber, TipoNumber) -> typeClash ".-." e1 e2
              (TipoNumber, _)          -> tipoExp' e2
              (_, TipoNumber)          -> tipoExp' e1
              _                        -> typeClash ".-." e1 e2


        tipoExp' (MultiplicacionCruzada e1 e2)
            = case (tipoExp' e1, tipoExp' e2) of
              (TipoBoolean, _)         -> typeClash ".*." e1 e2
              (_, TipoBoolean)         -> typeClash ".*." e1 e2
              (TipoNumber, TipoNumber) -> typeClash ".*." e1 e2
              (TipoNumber, _)          -> tipoExp' e2
              (_, TipoNumber)          -> tipoExp' e1
              _                        -> typeClash ".*." e1 e2

        tipoExp' (DivisionExactaCruzada e1 e2)
            = case (tipoExp' e1, tipoExp' e2) of
              (TipoBoolean, _)         -> typeClash "./." e1 e2
              (_, TipoBoolean)         -> typeClash "./." e1 e2
              (TipoNumber, TipoNumber) -> typeClash "./." e1 e2
              (TipoNumber, _)          -> tipoExp' e2
              (_, TipoNumber)          -> tipoExp' e1
              _                        -> typeClash "./." e1 e2

        tipoExp' (RestoExactoCruzado e1 e2)
            = case (tipoExp' e1, tipoExp' e2) of
              (TipoBoolean, _)         -> typeClash ".%." e1 e2
              (_, TipoBoolean)         -> typeClash ".%." e1 e2
              (TipoNumber, TipoNumber) -> typeClash ".%." e1 e2
              (TipoNumber, _)          -> tipoExp' e2
              (_, TipoNumber)          -> tipoExp' e1
              _                        -> typeClash ".%." e1 e2

        tipoExp' (DivisionEnteraCruzada e1 e2)
            = case (tipoExp' e1, tipoExp' e2) of
              (TipoBoolean, _)         -> typeClash ".div." e1 e2
              (_, TipoBoolean)         -> typeClash ".div." e1 e2
              (TipoNumber, TipoNumber) -> typeClash ".div." e1 e2
              (TipoNumber, _)          -> tipoExp' e2
              (_, TipoNumber)          -> tipoExp' e1
              _                        -> typeClash ".div." e1 e2

        tipoExp' (RestoEnteroCruzado e1 e2)
            = case (tipoExp' e1, tipoExp' e2) of
              (TipoBoolean, _)         -> typeClash ".mod." e1 e2
              (_, TipoBoolean)         -> typeClash ".mod." e1 e2
              (TipoNumber, TipoNumber) -> typeClash ".mod." e1 e2
              (TipoNumber, _)          -> tipoExp' e2
              (_, TipoNumber)          -> tipoExp' e1
              _                        -> typeClash ".mod." e1 e2

        tipoExp' (And e1 e2)
            = if tipoExp' e1 == TipoBoolean && tipoExp' e2 == TipoBoolean
                then TipoBoolean
                else typeClash "&" e1 e2

        tipoExp' (Or e1 e2)
            = if tipoExp' e1 == TipoBoolean && tipoExp' e2 == TipoBoolean
                then TipoBoolean
                else typeClash "|" e1 e2

        tipoExp' (Igual e1 e2)
            = if tipoExp' e1 == tipoExp' e2
                then TipoBoolean
                else typeClash "==" e1 e2

        tipoExp' (NoIgual e1 e2)
            = if tipoExp' e1 == tipoExp' e2
                then TipoBoolean
                else typeClash "/=" e1 e2

        tipoExp' (MenorIgual e1 e2)
            = if tipoExp' e1 == TipoNumber && tipoExp' e2 == TipoNumber
                then TipoBoolean
                else typeClash "<=" e1 e2

        tipoExp' (MayorIgual e1 e2)
            = if tipoExp' e1 == TipoNumber && tipoExp' e2 == TipoNumber
                then TipoBoolean
                else typeClash ">=" e1 e2

        tipoExp' (Menor e1 e2)
            = if tipoExp' e1 == TipoNumber && tipoExp' e2 == TipoNumber
                then TipoBoolean
                else typeClash "<" e1 e2

        tipoExp' (Mayor e1 e2)
            = if tipoExp' e1 == TipoNumber && tipoExp' e2 == TipoNumber
                then TipoBoolean
                else typeClash ">" e1 e2

        tipoExp' (ProyeccionMatriz e1 e2 e3) --number, matrix number number
            = if tipoExp' e2 == TipoNumber && tipoExp' e3 == TipoNumber
                then case tipoExp' e1 of
                     TipoMatriz _ _ -> TipoNumber
                     _ -> error $ "Type clash en proyeccion matricial: no es de tipo matricial"
                else error $ "Los argumentos de una proyeccion matricial deben ser numeros"

        tipoExp' (ProyeccionVector e1 e2) --number, (row o col) number
            = if tipoExp' e2 == TipoNumber
                then case tipoExp' e1 of
                     TipoMatriz 1 _ -> TipoNumber
                     TipoMatriz _ 1 -> TipoNumber
                     _ -> error $ "Type clash en proyeccion vectorial: no es de tipo vectorial"
                else error $ "El argumento de una proyeccion vectorial debe ser un numero"

        tipoExp' (Traspuesta e)
            = case tipoExp' e of
                (TipoMatriz f c) -> (TipoMatriz c f)
                _                -> typeClash'' "'" e

        tipoExp' (Negacion e)
            = if tipoExp' e /= TipoBoolean 
                then tipoExp' e
                else typeClash' "-" e

        tipoExp' (Not e)
            = if tipoExp' e == TipoBoolean
                then TipoBoolean
                else typeClash' "not" e

        tipoExp' (LiteralMatricial e) -- e = [[Expresion]]
            = case e of
                [] -> error $ "Literal matricial vacio"
                (columna:columnas)
                  -> if all (==length columna) $ length <$> columnas
                     then if all (==TipoNumber) $ concat $ (map . map) (tipoExp') e
                            then TipoMatriz (fromIntegral $ length columna) (fromIntegral $ length e)
                            else error $ "Elemento no numerico dentro de literal matricial " ++ show e
                     else error $ "Columnas de longitudes desiguales en matriz " ++ show e
                        
        tipoExp' (LlamadaFuncion token e) -- e = [Expresion]
            = verifyFuncion token e alcances funciones


        tipoExp' (LiteralNumerico _)
            = TipoNumber

        tipoExp' (Identificador token)
            = case definiciones of
                 []     -> errorPos "Variable no declarada"
                 _      -> head definiciones
              where
                var = token_string token
                definiciones = findDef alcances var
                errorPos e
                  = error $ e
                  ++ " en línea " ++ show linea
                  ++ ", columna " ++ show columna
                  where
                    Posicion linea columna = token_posn token

        tipoExp' (Falso)
            = TipoBoolean

        tipoExp' (Verdadero)
            = TipoBoolean

        typeClash op e1 e2 
            = error $ "Type clash en operacion " ++ show e1 ++ op ++ show e2

        typeClash' op e
            = error $ "Type clash en operacion " ++ op ++ show e

        typeClash'' op e
            = error $ "Type clash en operacion " ++ show e ++ op


verifyFuncion :: Token -> [Expresion] -> [Alcance] -> [Funcion] -> Tipo
verifyFuncion token e alcances funciones
    = do
        let funcName = token_string token
        let names = map getNameFuncion funciones
        let funcExists = filter (==funcName) names
        case funcExists of
            [] -> errorPos "Llamada a funcion no definida"
            _  -> verifyParams e alcances funciones (getFunc token funciones)
        where
            errorPos e
              = error $ e
              ++ " en línea " ++ show linea
              ++ ", columna " ++ show columna
              where
                Posicion linea columna = token_posn token

getFunc :: Token -> [Funcion] -> Funcion
getFunc id funciones
    = head $ [ x | x@(Funcion t _ _ _) <- funciones, token_string id == token_string t]

verifyParams :: [Expresion] -> [Alcance] -> [Funcion] -> Funcion -> Tipo
verifyParams e alcances funciones (Funcion id parametros tipo _)
    = do 
        let argTypes = map (tipoExp alcances funciones) e
        let paramTypes = map getTypeParametro parametros
        if length argTypes /= length paramTypes
            then error $ "Numero incorrectos de argumentos en llamada a funcion " ++ token_string id
            else if argTypes == paramTypes
                    then tipo
                    else error $ "Type clash en argumentos en llamada a funcion " ++ token_string id


checkFunctions :: Program -> Bool
checkFunctions (Program funciones _)
    = checkDuplicates getNameFuncion funciones
    && checkParam `all` funciones
    where
        checkParam :: Funcion -> Bool
        checkParam (Funcion _ parametros _ _)
            = checkDuplicates getNameParametro parametros

checkDuplicates :: Ord b => (a -> b) -> [a] -> Bool
checkDuplicates f l
    = length l == Set.size (Set.fromList $ map f l)

getNameParametro :: Parametro -> String
getNameParametro (Parametro _ i)
    = token_string i

getTypeParametro :: Parametro -> Tipo
getTypeParametro (Parametro t _)
    = t

getNameFuncion :: Funcion -> String
getNameFuncion (Funcion i _ _ _)
    = token_string i
