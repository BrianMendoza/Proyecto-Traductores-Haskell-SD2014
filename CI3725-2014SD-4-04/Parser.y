-- CI 3725 Septiembre-Diciembre 2014
-- Proyecto, Entrega 4
-- Integrantes:
--    Brian Mendoza 07-41206

{
module Parser where

import Tokens
import AST
import Lexer
import Data.Sequence (singleton, (|>), empty)
import Data.Foldable (toList)
}

%name parseTrinity
%tokentype { Token }
%error { parseError }

%token
  program   { TokenPrograma _ }
  boolean   { TokenBoolean _ }
  false     { TokenFalso _ }
  true      { TokenVerdadero _ }
  number    { TokenNumber _ }
  use       { TokenUse _ }
  in        { TokenIn _ }  
  end       { TokenEnd _ }
  set       { TokenSet _ }
  read      { TokenRead _ }  
  if        { TokenIf _ }
  then      { TokenThen _ }
  else      { TokenElse _ }
  for       { TokenFor _ }
  do        { TokenDo _ }  
  while     { TokenWhile _ }
  function  { TokenFunction _ }
  return    { TokenReturn _ }
  begin     { TokenBegin _ }
  matrix    { TokenMatrix _ }
  row       { TokenRow _ }
  col       { TokenCol _ }
  not       { TokenNot _ }
  div       { TokenDiv _ }
  mod       { TokenMod _ }
  '.div.'   { TokenDivCruz _ }
  '.mod.'   { TokenModCruz _ }
  print     { TokenPrint _ }  
  '('       { TokenLeftPar _ }
  ')'       { TokenRightPar _ }
  '{'       { TokenLeftLlave _ }
  '}'       { TokenRightLlave _ }
  '['       { TokenLeftCorchete _ }  
  ']'       { TokenRightCorchete _ }  
  ':'       { TokenDosPuntos _ }
  ','       { TokenComa _ }
  ';'       { TokenPuntoComa _ }
  '&'       { TokenAnd _ }
  '|'       { TokenOr _ }  
  '=='      { TokenIgual _ }
  '/='      { TokenNoIgual _ }
  '<='      { TokenMenorIgual _ }
  '>='      { TokenMayorIgual _ }
  '<'       { TokenMenor _ }
  '>'       { TokenMayor _ }
  '='       { TokenAsignar _ }
  '.+.'     { TokenPlusCruz _ }
  '+'       { TokenPlus _ }
  '.-.'     { TokenMinusCruz _ }
  '-'       { TokenMinus _ }
  '.*.'     { TokenMultiCruz _ }
  '*'       { TokenMulti _ }
  './.'     { TokenDividirCruz _ }
  '/'       { TokenDividir _ }
  '.%.'     { TokenModuloCruz _ }
  '%'       { TokenModulo _ }
  tra       { TokenComillaSimple _ }  
  num       { TokenNumLit _ _ }
  str       { TokenStrLit _ _ }
  id        { TokenIdent _ _ }


%left '|'
%left '&'
%nonassoc '==' '/=' '<=' '>=' '<' '>'
%right not
%left '+' '-' '.+.' '.-.'
%left '*' '/' '%' div mod '.*.' './.' '.%.' '.div.' '.mod.'
%right NEG
%left '[' tra


%%

Programa    :  Funciones program Instrucciones end ';'       { Program $1 $3 } --[Funcion] [Instruccion]

Expresion   :  Expresion '+' Expresion                       { Suma $1 $3 }
            |  Expresion '-' Expresion                       { Resta $1 $3 }
            |  Expresion '*' Expresion                       { Multiplicacion $1 $3 }
            |  Expresion '/' Expresion                       { DivisionExacta $1 $3 }
            |  Expresion '%' Expresion                       { RestoExacto $1 $3 }
            |  Expresion div Expresion                       { DivisionEntera $1 $3 }
            |  Expresion mod Expresion                       { RestoEntero $1 $3 }
            |  Expresion '.+.' Expresion                     { SumaCruzada $1 $3 }
            |  Expresion '.-.' Expresion                     { RestaCruzada $1 $3 }
            |  Expresion '.*.' Expresion                     { MultiplicacionCruzada $1 $3 }
            |  Expresion './.' Expresion                     { DivisionExactaCruzada $1 $3 }
            |  Expresion '.%.' Expresion                     { RestoExactoCruzado $1 $3 }
            |  Expresion '.div.' Expresion                   { DivisionEnteraCruzada $1 $3 }
            |  Expresion '.mod.' Expresion                   { RestoEnteroCruzado $1 $3 }
            |  Expresion '&' Expresion                       { And $1 $3 }
            |  Expresion '|' Expresion                       { Or $1 $3 }
            |  Expresion '==' Expresion                      { Igual $1 $3 }
            |  Expresion '/=' Expresion                      { NoIgual $1 $3 }
            |  Expresion '<=' Expresion                      { MenorIgual $1 $3 }
            |  Expresion '>=' Expresion                      { MayorIgual $1 $3 }
            |  Expresion '<' Expresion                       { Menor $1 $3 }
            |  Expresion '>' Expresion                       { Mayor $1 $3 }
            |  Expresion '[' Expresion ',' Expresion ']'     { ProyeccionMatriz $1 $3 $5 }
            |  Expresion '[' Expresion ']'                   { ProyeccionVector $1 $3 }
            |  Expresion tra                                 { Traspuesta $1 }
            |  '(' Expresion ')'                             { $2 }
            |  '-' Expresion %prec NEG                       { Negacion $2 }
            |  not Expresion                                 { Not $2 }
            |  '{'  Filas '}'                                { LiteralMatricial $2 } -- [[Expresion]]
            |   id '(' Argumentos ')'                        { LlamadaFuncion $1 $3 } -- [Expresion]
            |  num                                           { LiteralNumerico $1 }
            |  id                                            { Identificador $1 }
            |  true                                          { Verdadero }
            |  false                                         { Falso }

Instruccion :  set Lvalue '=' Expresion                                 { Asignacion $2 $4 }
            |  if Expresion then Instrucciones else Instrucciones end   { IfThenElse $2 $4 $6} -- [Instruccion]
            |  if Expresion then Instrucciones end                      { IfThen $2 $4 } --igual
            |  read id                                                  { Read $2 }
            |  print Impresiones                                        { Print $2 } --[Impresiones]
            |  use Declaraciones in Instrucciones end                   { Use $2 $4 } --[Declaracion] [Instruccion]
            |  while Expresion do Instrucciones end                     { While $2 $4 }
            |  for id in Expresion do Instrucciones end                 { For $2 $4 $6 }
            |  return Expresion                                         { Return $2 }
            |  Expresion                                                { InstruccionExpresion $1 }

Lvalue      :  id                                   { LValueIdentificador $1 }
            |  id '[' Expresion ']'                 { LValueVector $1 $3 }
            |  id '[' Expresion ',' Expresion ']'   { LValueMatriz $1 $3 $5 }

Impresion   :  Expresion  { ImprimirExpresion $1 }
            |  str        { ImprimirString $1 }

Declaracion :  Tipo id '=' Expresion  { DeclaracionInicializada $1 $2 $4 }
            |  Tipo id                { DeclaracionNoInicializada $1 $2 }

Funcion     :  function id '(' Parametros ')' return Tipo begin Instrucciones end ';' { Funcion $2 $4 $7 $9 } -- Token [Param] Tipo [Instruccion]

Parametro   :   Tipo id  { Parametro $1 $2 }

Tipo        :  boolean                     { TipoBoolean }
            |  number                      { TipoNumber }
            |  matrix '(' num ',' num ')'  { TipoMatriz (token_num $3) (token_num $5) }
            |  row '(' num ')'             { TipoMatriz (token_num $3) 1 }
            |  col '(' num ')'             { TipoMatriz 1 (token_num $3) }

SParametros: SParametros1 { $1 } | { empty }
SArgumentos: SArgumentos1 { $1 } | { empty }
SFunciones : SFunciones1  { $1 } | { empty }

SParametros1  : SParametros1    ',' Parametro       { $1 |> $3 } | Parametro { singleton $1 }
SArgumentos1  : SArgumentos1    ',' Expresion       { $1 |> $3 } | Expresion { singleton $1 }
SFilas        : SFilas          ':' Fila            { $1 |> $3 } | Fila      { singleton $1 }
SFila         : SFila           ',' Expresion       { $1 |> $3 } | Expresion { singleton $1 }
SImpresiones  : SImpresiones    ',' Impresion       { $1 |> $3 } | Impresion { singleton $1 }
SFunciones1   : SFunciones1         Funcion         { $1 |> $2 } | Funcion   { singleton $1 }
SDeclaraciones: SDeclaraciones      Declaracion ';' { $1 |> $2 } |           { empty }
SInstrucciones: SInstrucciones      Instruccion ';' { $1 |> $2 } |           { empty }

Parametros   : SParametros    { toList $1 }
Argumentos   : SArgumentos    { toList $1 }
Filas        : SFilas         { toList $1 }
Fila         : SFila          { toList $1 }
Impresiones  : SImpresiones   { toList $1 }
Funciones    : SFunciones     { toList $1 }
Declaraciones: SDeclaraciones { toList $1 }
Instrucciones: SInstrucciones { toList $1 }

{

parseError :: [Token] -> a
parseError l 
  = case l of 
    [] -> error $ "Unexpected EOF, parse error. Sugerencia: puede que falte un end;"
    _  -> error $ "Parse error en linea: " ++ show(linea) ++ ", columna: " ++ show(columna) ++ ", antes de " ++ (token_name $ head l)
              where Posicion linea columna = token_posn $ head l
}
