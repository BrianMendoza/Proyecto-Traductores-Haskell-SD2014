-- CI 3725 Septiembre-Diciembre 2014
-- Proyecto, Entrega 4
-- Integrantes:
--    Brian Mendoza 07-41206

{-# LANGUAGE DeriveGeneric #-}

module AST where

import Tokens
import GHC.Generics (Generic)
import Control.DeepSeq.Generics

instance NFData Tipo where rnf = genericRnf

data Expresion =
    Suma                     Expresion      Expresion                 |
    Resta                    Expresion      Expresion                 |
    Multiplicacion           Expresion      Expresion                 |
    DivisionExacta           Expresion      Expresion                 |
    RestoExacto              Expresion      Expresion                 |
    DivisionEntera           Expresion      Expresion                 |
    RestoEntero              Expresion      Expresion                 |
    SumaCruzada              Expresion      Expresion                 |
    RestaCruzada             Expresion      Expresion                 |
    MultiplicacionCruzada    Expresion      Expresion                 |
    DivisionExactaCruzada    Expresion      Expresion                 |
    RestoExactoCruzado       Expresion      Expresion                 |
    DivisionEnteraCruzada    Expresion      Expresion                 |
    RestoEnteroCruzado       Expresion      Expresion                 |
    And                      Expresion      Expresion                 |
    Or                       Expresion      Expresion                 |
    Igual                    Expresion      Expresion                 |
    NoIgual                  Expresion      Expresion                 |
    MenorIgual               Expresion      Expresion                 |
    MayorIgual               Expresion      Expresion                 |
    Menor                    Expresion      Expresion                 |
    Mayor                    Expresion      Expresion                 |
    ProyeccionMatriz         Expresion      Expresion     Expresion   |
    ProyeccionVector         Expresion      Expresion                 |
    Traspuesta               Expresion                                |
    Negacion                 Expresion                                |
    Not                      Expresion                                |
    LiteralMatricial       [[Expresion]]                              |
    LlamadaFuncion             Token       [Expresion]                |
    LiteralNumerico            Token                                  |
    Identificador              Token                                  |
    Falso                                                             |
    Verdadero
    deriving (Eq, Show)


data Instruccion =
    Asignacion               LValue         Expresion                     |
    IfThenElse              Expresion     [Instruccion]   [Instruccion]   |
    IfThen                  Expresion     [Instruccion]                   |
    Read                      Token                                       |
    Print                  [Impresion]                                    |
    Use                   [Declaracion]   [Instruccion]                   |
    While                   Expresion     [Instruccion]                   |
    For                       Token         Expresion     [Instruccion]   |
    Return                  Expresion                                     |
    InstruccionExpresion    Expresion
    deriving (Eq, Show)

data LValue =
    LValueIdentificador  Token                        |
    LValueVector         Token  Expresion             |
    LValueMatriz         Token  Expresion  Expresion
    deriving (Eq, Show)

data Impresion =
    ImprimirExpresion  Expresion  |
    ImprimirString     Token
    deriving (Eq, Show)

data Declaracion =
    DeclaracionInicializada   Tipo  Token  Expresion  |
    DeclaracionNoInicializada Tipo  Token
    deriving (Eq, Show)

data Funcion =
    Funcion Token [Parametro] Tipo [Instruccion]
    deriving (Eq, Show)

data Parametro = 
    Parametro Tipo Token
    deriving(Eq, Show)

data Program =
    Program [Funcion] [Instruccion]
    deriving(Eq, Show)

data Tipo =
    TipoBoolean                |
    TipoNumber                 |
    TipoMatriz  Double  Double
    deriving (Eq, Show, Generic)



