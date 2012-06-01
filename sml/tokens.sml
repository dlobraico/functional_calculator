(* tokens.sml *)

(* tokenization (aka lexical analysis) breaks down a sequence of characters 
 * into meaningful units like number literals, identifiers, keywords, operator
 * symbols, and punctuation marks like parentheses.  Here the tokenizer is
 * implemented in terms of a reader monad. *)

signature TOKENS =
sig
  type 'a reader

  val charsToString : char list reader -> string reader
  
  val isAlpha : char reader
  val isAlphaNum : char reader
  val isDigit : char reader
  val isSpace : char reader
  val nonSpace : char reader

  val ident : string reader
  val nat : int reader
  val space : unit reader

  val token : 'a reader -> 'a reader

  val identifier : string reader  (* token variant *)
  val natural : int reader        (* token variant *)

  datatype token
    = ID of string
    | NAT of int
    | NEG
    | PLUS | MINUS       (* additive operators (+, -) *)
    | TIMES | DIV | MOD  (* multiplicative operators ( *, /, %) *)
    | EQUAL | NOTEQUAL | LESS | GREATER | LESSEQ | GREATEREQ
        (* relational operators *)
    | TRUE | FALSE       (* boolean constants *)
    | AND | OR | NOT     (* boolean operators *)
    | LPAR | RPAR        (* left and right parentheses *)
    | LET | FUN | EQ     (* declaration keywords and punctuation *)
    | IF | THEN | ELSE   (* conditional expr keywords *)
    | SEMI

  type tokenStream = token list

  val tokenize : token list reader

end


structure Tokens : TOKENS =
struct

  open Reader

  (* sat : (char -> bool) -> char reader *)
  (* return next char if it satisfies prop, otherwise fail immediately *)
  fun sat (prop : char -> bool) : char reader =
    chain charReader (fn c => if prop c then return c else fail)

  (* checkChar : char -> char reader 
   * check that the next character available is c, then return c *)
  fun checkChar c = 
    sat (fn c' => c = c')

  (* checkChars: char list -> char list reader
   * check that the instream starts with s, then return s *) 
  fun checkChars ([] : char list) = return []
    | checkChars (s as (c::cs)) =
      chain (checkChar c) (fn _ => chain (checkChars cs) (fn _ => (return s)))  

  (* charsToString : char list reader -> string reader *)
  (* it's convenient to collect characters in lists, but we often want
   * strings in the end. (in Haskell, these are the same thing). *)
  fun charsToString (r : char list reader) : string reader = 
    chain r (fn c => return (implode c))
    

  val isAlpha : char reader = sat Char.isAlpha;
  val isAlphaNum : char reader = sat Char.isAlphaNum;
  val isDigit : char reader = sat (Char.isDigit);
  val isSpace : char reader = sat Char.isSpace
  val nonSpace : char reader = sat (not o Char.isSpace)

  (* ident -- alphanumeric identifiers, an alpha char followed by alphanumerics *)
  val ident : string reader = 
      chain isAlpha (fn c => 
       chain (star isAlphaNum) (fn cs =>
	return (implode(c::cs))));

  val nat : int reader = 
      chain (starPlus isDigit) (fn cs =>
       return(valOf(Int.fromString(implode cs))));

  val space : unit reader =
      chain (star isSpace) (fn _ =>
       return ())

  (* tokenizing -- space separated tokens *)

  fun token (r: 'a reader) : 'a reader =
      chain space (fn () => 
       chain r (fn v => 
	chain space (fn () =>
	 return v)))

  val identifier : string reader = token ident
  val natural : int reader = token nat

  fun checkString (s: string) = checkChars (explode s)
  fun symbol s = token (checkString s)

  fun keyword (s: string) : char list reader =
    token(checkString(s))

  datatype token
    = ID of string
    | NAT of int
    | NEG
    | PLUS | MINUS       (* additive operators (+, -) *)
    | TIMES | DIV | MOD  (* multiplicative operators ( *, /, %) *)
    | EQUAL | NOTEQUAL | LESS | GREATER | LESSEQ | GREATEREQ
        (* relational operators *)
    | TRUE | FALSE       (* boolean constants *)
    | AND | OR | NOT     (* boolean operators *)
    | LPAR | RPAR        (* left and right parentheses *)
    | LET | FUN | EQ     (* declaration keywords and punctuation *)
    | IF | THEN | ELSE   (* conditional expr keywords *)
    | SEMI

  type tokenStream = token list

  val +++ = Reader.choice
  infixr 6 +++

(* tokens: including ...
 *   natural number literals
 *   keywords and constant symbols: let, fun, if, then, else, not
 *   identitiers (variables bound to numbers or functions)
 *   operator symbols (arithmetic, relational, and boolean)
 *   left and right parentheses
 *  check for these in the indicated order (to make sure e.g. that
 *    "let" is not treated as an identifier instead of a keyword).
 *)

  val tokenRdr : token reader =
    chain natural (fn n =>
     return (NAT n))
    +++
    chain (keyword "let") (fn _ => 
     return LET)
    +++
    chain (keyword "fun") (fn _ => 
     return FUN)
    +++
    chain (keyword "if") (fn _ => 
     return IF)
    +++
    chain (keyword "then") (fn _ => 
     return THEN)
    +++
    chain (keyword "else") (fn _ => 
     return ELSE)
    +++
    chain (keyword "true") (fn _ =>
     return TRUE)
    +++
    chain (keyword "false") (fn _ =>
     return FALSE)
    +++
    chain (keyword "not") (fn _ =>
     return NOT)
    +++
    chain identifier (fn s =>
     return (ID s))
    +++
    chain (symbol "==") (fn _ =>
     return EQUAL)
    +++
    chain (symbol "/=") (fn _ =>
     return NOTEQUAL)
    +++
    chain (symbol "<=") (fn _ =>
     return LESSEQ)
    +++
    chain (symbol ">=") (fn _ =>
     return GREATEREQ)
    +++
    chain (symbol "&&") (fn _ =>
     return AND)
    +++
    chain (symbol "||") (fn _ =>
     return OR)
    +++
    chain (symbol "=") (fn _ => 
     return EQ)
    +++
    chain (symbol "<") (fn _ =>
     return LESS)
    +++
    chain (symbol ">") (fn _ =>
     return GREATER)
    +++
    chain (symbol "+") (fn _ =>
     return PLUS)
    +++
    chain (symbol "-") (fn _ =>
     return MINUS)
    +++
    chain (symbol "*") (fn _ =>
     return TIMES)
    +++
    chain (symbol "/") (fn _ =>
     return DIV)
    +++
    chain (symbol "%") (fn _ =>
     return MOD)
    +++
    chain (symbol "~") (fn _ =>
     return NEG)
    +++
    chain (symbol "(") (fn _ =>
     return LPAR)
    +++
    chain (symbol ")") (fn _ => 
     return RPAR)
    +++
    chain (symbol ";") (fn _ => 
     return SEMI)

  (* tokenize : tokenStream reader *)
  val tokenize = star tokenRdr

end (* structure TokensFn *)

(* Notes

1. Order of token reading
We have to try to read keywords (or reserved words) before reading identifiers
in general. Otherwise the keywords will be read as identifiers.

Similarly, we have to read two character symbol tokens like "==" and "<=" before 
reading single charater tokens that are prefixes, like "=" and "<".  Otherwise the
single character prefixes will be read first, and "==" will be read as two successive
"=" tokens.

*)

