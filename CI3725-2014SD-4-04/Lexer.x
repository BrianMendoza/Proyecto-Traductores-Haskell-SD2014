-- CI 3725 Septiembre-Diciembre 2014
-- Proyecto, Entrega 4
-- Integrantes:
--		Brian Mendoza 07-41206

{

module Lexer (AlexPosn(..), alexScanTokens, token_posn) where

import Tokens
}

%wrapper "posn"

$digit = 0-9 -- digits
$alpha = [a-zA-Z] -- alphabetic characters

tokens :-
  $white+                         ;
  \#.*                            ;
  program                         { tok TokenPrograma }
  boolean                         { tok TokenBoolean }
  false                           { tok TokenFalso }
  true                            { tok TokenVerdadero }
  number                          { tok TokenNumber }
  use                             { tok TokenUse }
  in                              { tok TokenIn }
  end                             { tok TokenEnd }
  set                             { tok TokenSet }
  read                            { tok TokenRead }
  if                              { tok TokenIf }
  then                            { tok TokenThen }
  else                            { tok TokenElse }
  for                             { tok TokenFor }
  do                              { tok TokenDo }
  while                           { tok TokenWhile }
  function                        { tok TokenFunction }
  return                          { tok TokenReturn }
  begin                           { tok TokenBegin }
  matrix                          { tok TokenMatrix }
  row                             { tok TokenRow }
  col                             { tok TokenCol }
  not                             { tok TokenNot }
  div                             { tok TokenDiv }
  mod                             { tok TokenMod }
  \.div\.                         { tok TokenDivCruz }
  \.mod\.                         { tok TokenModCruz }
  print                           { tok TokenPrint }
  \(                              { tok TokenLeftPar }
  \)                              { tok TokenRightPar }
  \{                              { tok TokenLeftLlave }
  \}                              { tok TokenRightLlave }
  \[                              { tok TokenLeftCorchete }
  \]                              { tok TokenRightCorchete }
  :                               { tok TokenDosPuntos }
  \,                              { tok TokenComa }
  \;                              { tok TokenPuntoComa }
  &                               { tok TokenAnd }
  \|                              { tok TokenOr }
  ==                              { tok TokenIgual }
  \/=                             { tok TokenNoIgual }
  \<=                             { tok TokenMenorIgual }
  >=                              { tok TokenMayorIgual }
  \<                              { tok TokenMenor }
  >                               { tok TokenMayor }
  =                               { tok TokenAsignar }
  \.\+\.                          { tok TokenPlusCruz }
  \+                              { tok TokenPlus }
  \.\-\.                          { tok TokenMinusCruz }
  \-                              { tok TokenMinus }
  \.\*\.                          { tok TokenMultiCruz }
  \*                              { tok TokenMulti }
  \.\/\.                          { tok TokenDividirCruz }
  \/                              { tok TokenDividir }
  \.\%\.                          { tok TokenModuloCruz }
  \%                              { tok TokenModulo }
  '                               { tok TokenComillaSimple }
  $digit+\.?$digit*               { (. read) . tok' TokenNumLit }
  \"([^\\\"]|\\n|\\\"|\\\\)*\"    { tok' TokenStrLit }
  $alpha[$alpha $digit \_]*       { tok' TokenIdent }
  .                               { tok' TokenError }

{

tok f p s = f (toPosicion p)
tok' f p s = f (toPosicion p) s

toPosicion :: AlexPosn -> Posicion
toPosicion (AlexPn _ linea columna) = Posicion linea columna

}
