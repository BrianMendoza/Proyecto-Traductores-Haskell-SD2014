-- CI 3725 Septiembre-Diciembre 2014
-- Proyecto, Entrega 4
-- Integrantes:
--		Brian Mendoza 07-41206


-- ContT $ \ k -> runContT (execs instrucciones) (error "boom")

-- v <- eval e; ContT $ \ k -> pure v 

module Interprete where

import AST
import Contexto (getFunc)
import Control.Applicative ((<|>), (<$>), empty, pure)
import Control.Monad (when, void)
import Control.Monad.Cont (ContT(ContT), runContT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, get, put, modify, runStateT, evalStateT)
import Control.Monad.Trans.Class (lift)
import Data.Foldable
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Sequence as Seq hiding (replicate)
import Data.Traversable (traverse)
import System.Exit
import Text.Read (reads)
import Tokens

type Memoria = [Map.Map String Valor]

type Interprete a = ContT Valor (ReaderT [Funcion] (StateT Memoria IO)) a

--Interprete Valor = ConParametro [Funcion] (ConCosaMutable Memoria IO) Valor
--                 = [Funcion] -> ConCosaMutable Memoria IO Valor
--                 = [Funcion] -> Valor -> IO (Valor,Memoria)

data Valor
    = Booleano { getBool   ::    Bool    }
    | Numero   { getNum    ::   Double   }
    | Matriz   { getMatriz :: [[Double]] }
    | Nada
    deriving (Eq, Show)

--redondear :: (Fractional a, RealFrac a1) => a1 -> a
--redondear n = (fromInteger $ round $ n * (10^2)) / (10.0^^2)


maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
    [(x, "")] -> Just x
    _         -> Nothing

run :: Program -> IO ()
run (Program funciones instrucciones) = void $ flip evalStateT [] $ flip runReaderT funciones $ flip runContT (\ () -> pure $ error "boom") $ execs instrucciones

execs :: [Instruccion] -> Interprete ()
execs = Prelude.mapM_ exec

exec :: Instruccion -> Interprete ()
exec (Read token) -- no genera alcance
  = do
    valor <- eval (Identificador token)
    input <- liftIO getLine
    memoria <- get
    case valor of
      Booleano _
        -> do
            let
              newBool
                = case input of
                  "true"  -> Booleano True
                  "false" -> Booleano False
                  _       -> Nada

              newMem
                = toList
                $ Seq.update 
                  indice 
                  (Map.insert name newBool $ (memoria :: Memoria) !! indice)
                $ Seq.fromList (memoria :: Memoria)
                where
                  indice
                    = fromJust
                    $ List.findIndex
                      (Map.member name)
                      (memoria :: Memoria)
            if newBool == Nada
              then liftIO exitSuccess
              else put newMem

      Numero _ 
        -> do
            let
              newNum
                = case maybeRead input :: Maybe Double of
                  Just n -> Numero n
                  _      -> Nada

              newMem
                = toList
                $ Seq.update indice (Map.insert name newNum $ (memoria :: Memoria) !! indice)
                $ Seq.fromList (memoria :: Memoria)
                where
                  indice
                    = fromJust
                    $ List.findIndex
                      (Map.member name)
                      (memoria :: Memoria)
            if newNum == Nada
              then liftIO exitSuccess
              else put newMem
  where
    name = token_string token

exec (Use declaraciones instrucciones) -- si genera alcance
  = do
      oldMem <- get
      argVals <- mapM toVals declaraciones
      let
        names
          = List.map toNames declaraciones

        tupleList
          = List.zip names argVals

        newAlcance
          = Map.fromList tupleList

        newMem
          = [newAlcance] ++ oldMem

      put newMem
      execs instrucciones
      modify tail

      where
        toVals :: Declaracion -> Interprete Valor
        toVals decl
          = do
            expEval <- case decl of
                        DeclaracionInicializada _ _ exp  -> eval exp
                        DeclaracionNoInicializada tipo _ -> return $ 
                                                              case tipo of
                                                                TipoBoolean    -> Booleano $ False
                                                                TipoNumber     -> Numero $ 0.0
                                                                TipoMatriz f c -> Matriz $ replicate (floor f) $ replicate (floor c) 0.0
            return expEval

        toNames :: Declaracion -> String
        toNames decl
          = case decl of
              DeclaracionInicializada _ token _ -> token_string token
              DeclaracionNoInicializada _ token -> token_string token



exec (Asignacion lvalue expresion) -- no genera alcance
  = do
    expEval <- eval expresion
    memoria <- get
    fi <- case lvalue of
          LValueIdentificador _     -> return Nada
          LValueVector        _ _   -> return Nada
          LValueMatriz        _ f _ -> eval f

    co <- case lvalue of
          LValueIdentificador _     -> return Nada
          LValueVector        _ c   -> eval c
          LValueMatriz        _ _ c -> eval c

    let
      name
        = case lvalue of
          LValueIdentificador token     -> token_string token
          LValueVector        token _   -> token_string token
          LValueMatriz        token _ _ -> token_string token

      fila
        = case lvalue of
          LValueIdentificador _     -> -1
          LValueVector        _ _   -> 0
          LValueMatriz        _ _ _ -> floor $ (getNum fi) - 1

      columna
        = case lvalue of
          LValueIdentificador _     -> -1
          LValueVector        _ _   -> floor $ (getNum co) - 1
          LValueMatriz        _ _ _ -> floor $ (getNum co) - 1

      oldVal
        = Map.findWithDefault (Nada) name $ (memoria :: Memoria) !! indice
        where
          indice
            = fromJust
            $ List.findIndex
              (Map.member name)
              (memoria :: Memoria)

      newVal
        = case lvalue of
          LValueIdentificador _ -> expEval
          _                     -> let
                                      seqMatriz
                                        = Seq.fromList $ List.map Seq.fromList $ getMatriz oldVal
                                      newSeqMatriz
                                        = Seq.update 
                                          fila 
                                          ( Seq.update
                                            columna
                                            (getNum expEval)
                                            (Seq.index seqMatriz fila) )
                                          seqMatriz
                                   in
                                   Matriz $ List.map toList $ toList newSeqMatriz

      newMem
        = toList
            $ Seq.update
              indice
              (Map.insert name newVal $ (memoria :: Memoria) !! indice)
              $ Seq.fromList (memoria :: Memoria)
            where
              indice
                = fromJust
                $ List.findIndex
                  (Map.member name)
                  (memoria :: Memoria)
    put newMem      


exec (IfThenElse expresion instruccionesThen instruccionesElse) -- no genera alcance
  = do
    guarda <- eval expresion
    case guarda of
      Booleano True  -> execs instruccionesThen
      Booleano False -> execs instruccionesElse

exec (IfThen expresion instruccionesThen) -- no genera alcance
  = do
    guarda <- eval expresion
    case guarda of
      Booleano True  -> execs instruccionesThen
      Booleano False -> pure ()

exec (Print impresiones) -- no genera alcance, impresiones :: [Impresion]
  = do 
      Prelude.mapM_ imprimir impresiones
    where
      imprimir :: Impresion -> Interprete ()
      imprimir i
        = case i of
          ImprimirExpresion expresion ->  do
                                          val <- eval expresion
                                          case val of
                                           Matriz   m -> liftIO $ Prelude.mapM_ print m
                                           Numero   n -> liftIO $ print n
                                           Booleano b -> liftIO $ print b

          ImprimirString    token     -> liftIO $ putStr $ unescape $ (init . tail) $ token_string token

      unescape :: String -> String
      unescape ('\\' : 'n' : s) = '\n' : unescape s
      unescape ('\\' : '"' : s) = '"' : unescape s
      unescape ('\\' : '\\' : s) = '\\' : unescape s
      unescape (s' : s) = s' : unescape s
      unescape "" = ""


exec ciclo@(While expresion instrucciones) -- no genera alcance
  = do
    guarda' <- eval expresion
    let guarda = getBool guarda'
    when guarda $ do execs instrucciones; exec ciclo

exec (For identificador rango instrucciones) -- si genera alcance
  = do
    Matriz m <- eval rango
    let name = token_string identificador
    for_ (List.concat m) $ \ i -> do
      oldMem <- get
      put $ Map.singleton name (Numero i) : oldMem
      execs instrucciones
      modify tail

exec (Return expresion) -- no genera alcance
  = do
    v <- eval expresion
    ContT $ \ k -> pure v

exec (InstruccionExpresion expresion) -- no genera alcance
  = do 
      eval expresion
      return ()




eval :: Expresion -> Interprete Valor -- ask para funciones, get para memoria
eval (Suma e1 e2)
  = do
    r1 <- eval e1
    r2 <- eval e2
    return
      $ case (r1, r2) of
        (Numero n1, Numero n2) -> Numero (n1+n2)
        (Matriz m1, Matriz m2) -> Matriz ((zipWith . zipWith) (+) m1 m2)

eval (Resta e1 e2)
  = do
    r1 <- eval e1
    r2 <- eval e2
    return
      $ case (r1, r2) of
        (Numero n1, Numero n2) -> Numero (n1-n2)
        (Matriz m1, Matriz m2) -> Matriz ((zipWith . zipWith) (-) m1 m2)

eval (Multiplicacion e1 e2)
  = do
    r1 <- eval e1
    r2 <- eval e2
    return
      $ case (r1, r2) of
        (Numero n1, Numero n2) -> Numero (n1*n2)
        (Matriz m1, Matriz m2) -> Matriz ((zipWith . zipWith) (*) m1 m2)

eval (DivisionExacta e1 e2)
  = do
    r1 <- eval e1
    r2 <- eval e2
    let n1 = getNum r1
    let n2 = getNum r2
    if n2 == 0
      then liftIO exitSuccess
      else return
              $ Numero (n1 / n2)

eval (RestoExacto e1 e2)
  = do
    r1 <- eval e1
    r2 <- eval e2
    let n1 = getNum r1
    let n2 = getNum r2
    if n2 == 0
      then liftIO exitSuccess
      else return
              $ Numero (n1 - (fromIntegral $ floor $ n1 / n2)*n2)

eval (DivisionEntera e1 e2)
  = do
    r1 <- eval e1
    r2 <- eval e2
    let n1 = getNum r1
    let n2 = getNum r2
    if n2 == 0
      then liftIO exitSuccess
      else return
              $ Numero (fromIntegral $ floor $ n1 / n2)

eval (RestoEntero e1 e2)
  = do
    r1 <- eval e1
    r2 <- eval e2
    let n1 = getNum r1
    let n2 = getNum r2
    if n2 == 0
      then liftIO exitSuccess
      else return
              $ Numero (fromIntegral $ floor $ n1 - (fromIntegral $ floor $ n1 / n2)*n2)

eval (SumaCruzada e1 e2)
  = do
    r1 <- eval e1
    r2 <- eval e2
    return
      $ case (r1, r2) of
        (Numero n, Matriz m) -> Matriz $ (map . map) (+n) m
        (Matriz m, Numero n) -> Matriz $ (map . map) (+n) m

eval (RestaCruzada e1 e2)
  = do
    r1 <- eval e1
    r2 <- eval e2
    return
      $ case (r1, r2) of
        (Numero n, Matriz m) -> Matriz $ (map . map) (subtract n) m
        (Matriz m, Numero n) -> Matriz $ (map . map) (subtract n) m

eval (MultiplicacionCruzada e1 e2)
  = do
    r1 <- eval e1
    r2 <- eval e2
    return
      $ case (r1, r2) of
        (Numero n, Matriz m) -> Matriz $ (map . map) (*n) m
        (Matriz m, Numero n) -> Matriz $ (map . map) (*n) m

eval (DivisionExactaCruzada e1 e2)
  = do
    r1 <- eval e1
    r2 <- eval e2
    let res
          = case (r1, r2) of
            (Numero n, Matriz m) -> if n == 0
                                    then Nada
                                    else Matriz $ (map . map) (/n) m
            (Matriz m, Numero n) -> if n == 0
                                    then Nada
                                    else Matriz $ (map . map) (/n) m
    if res == Nada
      then liftIO exitSuccess
      else return res

eval (RestoExactoCruzado e1 e2)
  = do
    r1 <- eval e1
    r2 <- eval e2
    let res
          = case (r1, r2) of
              (Numero n, Matriz m) -> if n == 0
                                      then Nada
                                      else Matriz $ (map . map) (\ x -> x - (fromIntegral $ floor $ x/n)*n) m
              (Matriz m, Numero n) -> if n == 0
                                      then Nada
                                      else Matriz $ (map . map) (\ x -> x - (fromIntegral $ floor $ x/n)*n) m
    if res == Nada
      then liftIO exitSuccess
      else return res

eval (DivisionEnteraCruzada e1 e2)
  = do
    r1 <- eval e1
    r2 <- eval e2
    let res
          = case (r1, r2) of
              (Numero n, Matriz m) -> if n == 0
                                      then Nada
                                      else Matriz $ (map . map) (fromIntegral . floor . (/n)) m
              (Matriz m, Numero n) -> if n == 0
                                      then Nada
                                      else Matriz $ (map . map) (fromIntegral . floor . (/n)) m
    if res == Nada
      then liftIO exitSuccess
      else return res

eval (RestoEnteroCruzado e1 e2)
  = do
    r1 <- eval e1
    r2 <- eval e2
    let res
          = case (r1, r2) of
              (Numero n, Matriz m) -> if n == 0
                                      then Nada
                                      else Matriz $ (map . map) (\ x -> fromIntegral $ floor $ x - (fromIntegral $ floor $ x/n)*n) m
              (Matriz m, Numero n) -> if n == 0
                                      then Nada
                                      else Matriz $ (map . map) (\ x -> fromIntegral $ floor $ x - (fromIntegral $ floor $ x/n)*n) m
    if res == Nada
      then liftIO exitSuccess
      else return res

eval (And e1 e2)
  = do
    r1 <- eval e1
    let b1 = getBool r1
    if b1 == False
      then return $ Booleano False
      else do
            r2 <- eval e2
            let b2 = getBool r2
            return $ Booleano (b1 && b2)

eval (Or e1 e2)
  = do
    r1 <- eval e1
    let b1 = getBool r1
    if b1 == True
      then return $ Booleano True
      else do
            r2 <- eval e2
            let b2 = getBool r2
            return $ Booleano (b1 || b2)

eval (Igual e1 e2)
  = do
    r1 <- eval e1
    r2 <- eval e2
    return
      $ Booleano (r1 == r2)

eval (NoIgual e1 e2)
  = do
    r1 <- eval e1
    r2 <- eval e2
    return
      $ Booleano (r1 /= r2)

eval (MenorIgual e1 e2)
  = do
    r1 <- eval e1
    r2 <- eval e2
    let n1 = getNum r1
    let n2 = getNum r2
    return
      $ Booleano (n1 <= n2)

eval (MayorIgual e1 e2)
  = do
    r1 <- eval e1
    r2 <- eval e2
    let n1 = getNum r1
    let n2 = getNum r2
    return
      $ Booleano (n1 >= n2)

eval (Menor e1 e2)
  = do
    r1 <- eval e1
    r2 <- eval e2
    let n1 = getNum r1
    let n2 = getNum r2
    return
      $ Booleano (n1 < n2)

eval (Mayor e1 e2)
  = do
    r1 <- eval e1
    r2 <- eval e2
    let n1 = getNum r1
    let n2 = getNum r2
    return
      $ Booleano (n1 > n2)

eval (ProyeccionMatriz e1 e2 e3)
  = do
    r1 <- eval e1
    let m = getMatriz r1
    r2 <- eval e2
    if not $ esEntero $ getNum r2
      then liftIO exitSuccess
      else return ()
    let f = floor $ (getNum r2) - 1
    r3 <- eval e3
    if not $ esEntero $ getNum r3
      then liftIO exitSuccess
      else return ()
    let c = floor $ (getNum r3) - 1
    if f >= 0 && f < length m
      then if c >= 0 && c < length (m !! f)
             then return $ Numero (m !! f !! c)
             else liftIO exitSuccess
      else liftIO exitSuccess
    where
      esEntero n 
        = n - (fromIntegral $ floor n) == 0

eval (ProyeccionVector e1 e2)
  = do
    r1 <- eval e1
    let v = getMatriz r1
    r2 <- eval e2
    if not $ esEntero $ getNum r2
      then liftIO exitSuccess
      else return ()
    let n = floor $ (getNum r2) - 1
    case length v of
      1 -> if n >= 0 && n < length (head v)
             then return $ Numero (head v !! n)
             else liftIO exitSuccess
      _ -> if n >= 0 && n < length v
             then return $ Numero (head $ v !! n)
             else liftIO exitSuccess
    where
      esEntero n 
        = n - (fromIntegral $ floor n) == 0

eval (Traspuesta e)
  = do
    r <- eval e
    let m = getMatriz r
    return $ Matriz (List.transpose m)

eval (Negacion e)
  = do
    r <- eval e
    return
      $ case r of
        Numero n -> Numero $ -n
        Matriz m -> Matriz $ (map . map) (*(-1)) m

eval (Not e)
  = do
    r <- eval e
    let b = getBool r
    return
      $ Booleano $ not b

eval (LiteralMatricial e) -- e = [[Expresion]]
  = do
    m' <- sequence $ map sequence $ (map . map) eval e
    let m = (List.map . List.map) getNum m'
    return
      $ Matriz m

eval (LlamadaFuncion token e) -- e = [Expresion]
  = do
    funciones <- ask
    let (Funcion token' params _ instrucciones) = getFunc token funciones
    args <- sequence $ eval <$> e
    oldMem <- get
    let newMem = (genMem params args) ++ oldMem
    put newMem

    val <- lift $ execs instrucciones `runContT` \ () -> liftIO $ exitSuccess

    modify tail
    return val
    where
        genMem :: [Parametro] -> [Valor] -> Memoria
        genMem params argValues
            = do
              let paramNames = [ token_string t | (Parametro _ t) <- params]
              let tupleList = zip paramNames argValues
              [Map.fromList tupleList]


eval (LiteralNumerico token)
  = return
      $ Numero $ token_num token

eval (Identificador token)
  = do
    let var = token_string token
    mem <- get
    return
      $ head $ mapMaybe (Map.lookup var) mem

eval (Falso)
  = return
      $ Booleano False

eval (Verdadero)
  = return
      $ Booleano True


