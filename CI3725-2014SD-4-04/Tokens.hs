-- CI 3725 Septiembre-Diciembre 2014
-- Proyecto, Entrega 4
-- Integrantes:
--    Brian Mendoza 07-41206

module Tokens where

data Posicion =
    Posicion Int Int
    deriving (Eq, Show)

data Token =
    TokenPrograma       Posicion          |
    TokenBoolean        Posicion          |
    TokenFalso          Posicion          |
    TokenVerdadero      Posicion          |
    TokenNumber         Posicion          |
    TokenUse            Posicion          |
    TokenIn             Posicion          |
    TokenEnd            Posicion          |
    TokenSet            Posicion          |
    TokenRead           Posicion          |
    TokenIf             Posicion          |
    TokenThen           Posicion          |
    TokenElse           Posicion          |
    TokenFor            Posicion          |
    TokenDo             Posicion          |
    TokenWhile          Posicion          |
    TokenFunction       Posicion          |
    TokenReturn         Posicion          |
    TokenBegin          Posicion          |
    TokenMatrix         Posicion          |
    TokenRow            Posicion          |
    TokenCol            Posicion          |
    TokenNot            Posicion          |
    TokenDiv            Posicion          |
    TokenMod            Posicion          |
    TokenDivCruz        Posicion          |
    TokenModCruz        Posicion          |
    TokenPrint          Posicion          |
    TokenLeftPar        Posicion          |
    TokenRightPar       Posicion          |
    TokenLeftLlave      Posicion          |
    TokenRightLlave     Posicion          |
    TokenLeftCorchete   Posicion          |
    TokenRightCorchete  Posicion          |
    TokenDosPuntos      Posicion          |
    TokenComa           Posicion          |
    TokenPuntoComa      Posicion          |
    TokenAnd            Posicion          |
    TokenOr             Posicion          |
    TokenIgual          Posicion          |
    TokenNoIgual        Posicion          |
    TokenMenorIgual     Posicion          |
    TokenMayorIgual     Posicion          |
    TokenMenor          Posicion          |
    TokenMayor          Posicion          |
    TokenAsignar        Posicion          |
    TokenPlusCruz       Posicion          |
    TokenPlus           Posicion          |
    TokenMinusCruz      Posicion          |
    TokenMinus          Posicion          |
    TokenMultiCruz      Posicion          |
    TokenMulti          Posicion          |
    TokenDividirCruz    Posicion          |
    TokenDividir        Posicion          |
    TokenModuloCruz     Posicion          |
    TokenModulo         Posicion          |
    TokenComillaSimple  Posicion          |
    TokenNumLit         Posicion  Double  |
    TokenStrLit         Posicion  String  |
    TokenIdent          Posicion  String  |
    TokenError          Posicion  String
    deriving (Eq, Show) --last one without the |

isError :: Token -> Bool
isError t
  = case t of
    TokenError _ _ -> True
    _              -> False

noHayError :: [Token] -> Bool
noHayError l = null $ listErrors l

listErrors :: [Token] -> [Token]
listErrors l = filter isError l

token_num :: Token -> Double
token_num (TokenNumLit _ n) = n

token_string :: Token -> String
token_string (TokenStrLit _ s) = s
token_string (TokenIdent _ s) = s


token_posn :: Token -> Posicion
token_posn t 
  = case t of
    (TokenPrograma p)      -> p
    (TokenBoolean p)       -> p
    (TokenFalso p)         -> p
    (TokenVerdadero p)     -> p
    (TokenNumber p)        -> p
    (TokenUse p)           -> p
    (TokenIn p)            -> p
    (TokenEnd p)           -> p
    (TokenSet p)           -> p
    (TokenRead p)          -> p
    (TokenIf p)            -> p
    (TokenThen p)          -> p
    (TokenElse p)          -> p
    (TokenFor p)           -> p
    (TokenDo p)            -> p
    (TokenWhile p)         -> p
    (TokenFunction p)      -> p
    (TokenReturn p)        -> p
    (TokenBegin p)         -> p
    (TokenMatrix p)        -> p
    (TokenRow p)           -> p
    (TokenCol p)           -> p
    (TokenNot p)           -> p
    (TokenDiv p)           -> p
    (TokenMod p)           -> p
    (TokenDivCruz p)       -> p
    (TokenModCruz p)       -> p
    (TokenPrint p)         -> p
    (TokenLeftPar p)       -> p
    (TokenRightPar p)      -> p
    (TokenLeftLlave p)     -> p
    (TokenRightLlave p)    -> p
    (TokenLeftCorchete p)  -> p
    (TokenRightCorchete p) -> p
    (TokenDosPuntos p)     -> p
    (TokenComa p)          -> p
    (TokenPuntoComa p)     -> p
    (TokenAnd p)           -> p
    (TokenOr p)            -> p
    (TokenIgual p)         -> p
    (TokenNoIgual p)       -> p
    (TokenMenorIgual p)    -> p
    (TokenMayorIgual p)    -> p
    (TokenMenor p)         -> p
    (TokenMayor p)         -> p
    (TokenAsignar p)       -> p
    (TokenPlusCruz p)      -> p
    (TokenPlus p)          -> p
    (TokenMinusCruz p)     -> p
    (TokenMinus p)         -> p
    (TokenMultiCruz p)     -> p
    (TokenMulti p)         -> p
    (TokenDividirCruz p)   -> p
    (TokenDividir p)       -> p
    (TokenModuloCruz p)    -> p
    (TokenModulo p)        -> p
    (TokenComillaSimple p) -> p
    (TokenNumLit p _)      -> p
    (TokenStrLit p _)      -> p
    (TokenIdent p _)       -> p
    (TokenError p _)       -> p

token_name :: Token -> String
token_name t
  = case t of
    (TokenPrograma _)      -> "Palabra reservada: program"
    (TokenBoolean _)       -> "Palabra reservada: boolean"
    (TokenFalso _)         -> "Palabra reservada: false"
    (TokenVerdadero _)     -> "Palabra reservada: true"
    (TokenNumber _)        -> "Palabra reservada: number"
    (TokenUse _)           -> "Palabra reservada: use"
    (TokenIn _)            -> "Palabra reservada: in"
    (TokenEnd _)           -> "Palabra reservada: end"
    (TokenSet _)           -> "Palabra reservada: set"
    (TokenRead _)          -> "Palabra reservada: read"
    (TokenIf _)            -> "Palabra reservada: if"
    (TokenThen _)          -> "Palabra reservada: then"
    (TokenElse _)          -> "Palabra reservada: else"
    (TokenFor _)           -> "Palabra reservada: for"
    (TokenDo _)            -> "Palabra reservada: do"
    (TokenWhile _)         -> "Palabra reservada: while"
    (TokenFunction _)      -> "Palabra reservada: function"
    (TokenReturn _)        -> "Palabra reservada: return"
    (TokenBegin _)         -> "Palabra reservada: begin"
    (TokenMatrix _)        -> "Palabra reservada: matrix"
    (TokenRow _)           -> "Palabra reservada: row"
    (TokenCol _)           -> "Palabra reservada: col"
    (TokenNot _)           -> "Palabra reservada: not"
    (TokenDiv _)           -> "Palabra reservada: div escalar"
    (TokenMod _)           -> "Palabra reservada: mod escalar"
    (TokenDivCruz _)       -> "Palabra reservada: div cruzado"
    (TokenModCruz _)       -> "Palabra reservada: mod cruzado"
    (TokenPrint _)         -> "Palabra reservada: print"
    (TokenLeftPar _)       -> "Parentesis Izquierdo"
    (TokenRightPar _)      -> "Parentesis Derecho"
    (TokenLeftLlave _)     -> "Llave Izquierda"
    (TokenRightLlave _)    -> "Llave Derecha"
    (TokenLeftCorchete _)  -> "Corechete Izquierdo"
    (TokenRightCorchete _) -> "Corchete Derecho"
    (TokenDosPuntos _)     -> "Dos Puntos"
    (TokenComa _)          -> "Coma"
    (TokenPuntoComa _)     -> "Punto y Coma"
    (TokenAnd _)           -> "And Logico"
    (TokenOr _)            -> "Or Logico"
    (TokenIgual _)         -> "Comparacion por igualdad"
    (TokenNoIgual _)       -> "Comparacion por desigualdad"
    (TokenMenorIgual _)    -> "Comparacion menor o igual"
    (TokenMayorIgual _)    -> "Comparacion mayor o igual"
    (TokenMenor _)         -> "Comparacion menor"
    (TokenMayor _)         -> "Comparacion mayor"
    (TokenAsignar _)       -> "Asignacion"
    (TokenPlusCruz _)      -> "Operador: Suma cruzada"
    (TokenPlus _)          -> "Operador: Suma Escalar"
    (TokenMinusCruz _)     -> "Operador: Resta Cruzada"
    (TokenMinus _)         -> "Operador: Resta Escalar"
    (TokenMultiCruz _)     -> "Operador: Multiplicacion Cruzada"
    (TokenMulti _)         -> "Operador: Multiplicacion Escalar"
    (TokenDividirCruz _)   -> "Operador: Division Cruzada"
    (TokenDividir _)       -> "Operador: Division Escalar"
    (TokenModuloCruz _)    -> "Operador: Residuo Cruzado"
    (TokenModulo _)        -> "Operador: Residuo Escalar"
    (TokenComillaSimple _) -> "Comilla simple"
    (TokenNumLit _ _)      -> "Literal Numerico"
    (TokenStrLit _ _)      -> "String Literal"
    (TokenIdent _ _)       -> "Identificador"
    (TokenError _ _)       -> "Caracter inesperado: "




