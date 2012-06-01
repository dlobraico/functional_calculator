module Tokens where
import Reader

data Token = 
      ID String
    | NAT Int
    | NEG
    | PLUS | MINUS 
    | TIMES | DIV | MOD 
    | EQUAL | NOTEQUAL | LESS | GREATER | LESSEQ | GREATEREQ
    | TRUE | FALSE
    | AND | OR | NOT
    | LPAR | RPAR
    | LET | FUN | EQ
    | IF | THEN | ELSE
    | SEMI
    deriving Eq

type TokenStream = [Token]

instance Show Token where
  show tok =
    case tok of
      ID s -> concat["ID(", s, ")"]
      NAT i -> concat["INT(", show i, ")"]
      NEG -> "NEG"
      PLUS -> "PLUS"
      MINUS -> "MINUS"
      TIMES -> "TIMES"
      DIV -> "DIV"
      MOD -> "MOD"
      EQUAL -> "EQUAL"
      NOTEQUAL -> "NOTEQUAL"
      LESS -> "LESS"
      GREATER -> "GREATER"
      LESSEQ -> "LESSEQ"
      GREATEREQ -> "GREATEREQ"
      TRUE -> "TRUE"
      FALSE -> "FALSE"
      AND -> "AND"
      OR -> "OR"
      NOT -> "NOT"
      LPAR -> "LPAR"
      RPAR -> "RPAR"
      IF -> "IF"
      THEN -> "THEN"
      ELSE -> "ELSE"
      SEMI -> "SEMI"


tokenRdr :: Reader Token
tokenRdr =
    do n <- natural
       return (NAT n)
    +++
    do keyword "let"
       return LET
    +++
    do keyword "fun"
       return FUN
    +++
    do keyword "if"
       return IF
    +++
    do keyword "then"
       return THEN
    +++
    do keyword "else"
       return ELSE
    +++
    do keyword "true"
       return TRUE 
    +++
    do keyword "false"
       return FALSE 
    +++
    do keyword "not"
       return NOT 
    +++
    do s <- identifier 
       return (ID s)
    +++
    do symbol "=="
       return EQUAL
    +++
    do symbol "/="
       return NOTEQUAL
    +++
    do symbol "<="
       return LESSEQ
    +++
    do symbol ">="
       return GREATEREQ
    +++
    do symbol "&&"
       return AND
    +++
    do symbol "||"
       return OR
    +++
    do symbol "="
       return Tokens.EQ
    +++
    do symbol "<"
       return LESS
    +++
    do symbol ">"
       return GREATER
    +++
    do symbol "+" 
       return PLUS
    +++
    do symbol "-" 
       return MINUS
    +++
    do symbol "*" 
       return TIMES
    +++
    do symbol "/" 
       return DIV
    +++
    do symbol "%" 
       return MOD
    +++
    do symbol "~" 
       return NEG
    +++
    do symbol "(" 
       return LPAR
    +++
    do symbol ")" 
       return RPAR
    +++
    do symbol ";" 
       return SEMI

tokenize :: Reader TokenStream
tokenize = starPlus tokenRdr
