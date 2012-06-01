(* parser.sml *)

structure Parser =
struct

structure T = Tokens 
open Exprs

(* parser: analagous to reader, but consumes tokens instead of 
 * characters *)
type 'a parser = T.tokenStream -> ('a * T.tokenStream) option

(* return : 'a -> 'a parser *)
fun return (v: 'a) : 'a parser = fn toks => SOME(v, toks)

(* chain : 'a parser -> ('a -> 'b parser) -> 'b parser *)
fun chain (p: 'a parser) (f: 'a -> 'b parser) : 'b parser = 
    fn toks =>
       (case p toks
          of NONE => NONE
	   | SOME(a, toks') => 
               f a toks')

val fail : 'a parser = (fn (toks: T.tokenStream) => NONE)

(* getToken : token parser
 * analagous to getChar basic character reader, but for tokens *)
fun getToken (toks: T.tokenStream): (T.token * T.tokenStream) option =
    (case toks
       of [] => NONE
	| t::toks' => SOME (t,toks'))

(* choice operaotor for parsers *)
(* choice : 'a parser * 'a parser -> 'a parser *)
fun choice (p: 'a parser, q: 'a parser): 'a parser =
    (fn toks =>
        (case p toks
           of NONE => q toks
            | res => res))

(* grammar for expressions 
   -- This gives the full language with arithmetic expressions, boolean expressions,
      conditional expressions, simple let declarations and function declarations
      and function applications.  You can get started with just aexpr with no
      identifiers or function applications and gradually add the rest of the
      features.

empty  ::=

-- arithmetic expressions
aexpr   ::=  aterm ("+" aexpr | "-" aexpr | empty)
aterm   ::=  factor ("*" aterm | "/" aterm | "%" aterm | empty)
afactor ::=  "~" afactor | "(" cexpr ")" | id "(" cexpr ")" | nat | id
nat     ::=  Nat n    -- literal Nat token
id      ::=  Id s     -- literal Id token

-- relational expression
rexpr  ::=  aexpr "==" aexpr | aexpr "/=" aexpr | aexpr "<" aexpr | aexpr ">" aexpr 
        |   aexpr "<=" aexpr | aexpr ">=" aexpr 

-- boolean expressions
bexpr   ::=  bterm ("||" bexpr | empty)
bterm   ::=  bfactor ("&&" | empty)
bfactor ::=  "not" "(" bexpr ")" | "true" | "false" | cexpr

-- conditional expression
cexpr  ::=  "if" bexpr "then" bexpr "else" bexpr | aexpr

decl   ::=  "let" id "=" bexpr | "fun" id id = bexpr
stmt   ::=  decl | bexpr

These grammar rules can be translated fairly straightforwardly into
corresponding parser actions (see aexp for example).
*)

val +++ = choice
infixr 6 +++

fun relTokToOper tok =
    case tok
      of T.EQUAL => Equal
       | T.NOTEQUAL => NotEqual
       | T.LESS => Less
       | T.GREATER => Greater
       | T.LESSEQ => LessEq
       | T.GREATEREQ => GreaterEq
       | _ => raise Fail "unexpected relational operator"

(* isToken : token -> token parser *)
fun isToken (t: T.token) =
    chain getToken (fn t' =>
     if t = t' then return t
     else fail)

(* natExpr : expr parser *)
val natExpr =
    chain getToken (fn t =>
     case t
       of T.NAT n => return (Nat n)
        |  _ => fail)

(* identExpr : expr parser *)
val identExpr =
    chain getToken (fn t =>
     case t
       of T.ID s => return (Id s)
        | _ => fail)

(* variable = identifier token, but returns string, used to get
 * bound variables in declarations *)
val variable : string parser =
    chain getToken (fn t =>
     case t
       of T.ID s => return s
        | _ => fail)


(* definitions of the parser actions.
 * We need to define these as mutually recursive functions,
 * so we "eta-expand" them by abstracting over toks: tokenstream,
 * and then apply the appropriate parser action expression to
 * toks. *)

(* aexpr : expr parser *)
fun aexpr toks = 
    chain aterm (fn t =>
     chain (isToken T.PLUS) (fn _ =>
      chain aexpr (fn e =>
       return (BinaryApp(Plus, t, e))))
     +++
     chain (isToken T.MINUS) (fn _ =>
      chain aexpr (fn e =>
       return (BinaryApp(Minus, t, e))))
     +++
     return t) toks

(* aterm : expr parser *)
and aterm toks = 
    chain afactor (fn f =>
     chain (isToken T.TIMES) (fn _ =>
      chain aterm (fn t =>
       return (BinaryApp(Times, f, t))))
     +++
     chain (isToken T.DIV) (fn _ =>
      chain aterm (fn u =>
       return (BinaryApp(Div, f, u))))
     +++
     chain (isToken T.MOD) (fn _ =>
      chain aterm (fn v =>
       return (BinaryApp(Mod, f, v))))
     +++
     return f) toks


(* funApp : expr parser 
   -- function applications appearing as "id (cexpr)" in
      the grammar *)
and funApp toks = 
    chain variable (fn f => 
     chain (isToken T.LPAR) (fn _ =>
      chain cexpr (fn x =>
       chain (isToken T.RPAR) (fn _ =>
        return (App(f, x)))))) toks

(* afactor : expr parser *)
and afactor toks = 
    ((chain (isToken T.NEG)  (fn _ => 
       chain afactor (fn f => 
        return (UnaryApp(Neg,f)))))
    +++
    (chain (isToken T.LPAR) (fn _ => 
      chain cexpr (fn e => 
        chain (isToken T.RPAR) (fn _ => 
          return e))))
    +++
    funApp
    +++
    natExpr
    +++
    identExpr ) toks

(* rexpr: relational expressions -- atomic boolean expressions *)
(* rexpr: expr parser *)
and rexpr toks = 
   (chain aexpr (fn e1 =>
     chain ((isToken T.EQUAL) +++
            (isToken T.NOTEQUAL) +++
            (isToken T.LESS) +++
            (isToken T.GREATER) +++
            (isToken T.LESSEQ) +++
            (isToken T.GREATEREQ)) (fn reltok =>
      chain aexpr (fn e2 =>
       return (BinaryApp(relTokToOper reltok,e1,e2)))))
   ) toks

(* bexpr: boolean expressions formed by infix && and || and unary "not"
 * && has higher precedence than || and they are both right associative.
 * Subsumes rexpr. *)
and bexpr toks =
   (chain bterm (fn be1 =>
     chain (isToken T.OR) (fn _ =>
      chain bexpr (fn be2 => 
       return (BinaryApp(Or, be1, be2))))
     +++
     return be1)
   ) toks

and bterm toks = 
    (chain bfactor (fn be1 =>
     chain (isToken T.AND) (fn _ =>
      chain bterm (fn be2 =>
       return (BinaryApp(And, be1, be2))))
       +++
       return be1)
       ) toks

and bfactor toks =
    (rexpr
     +++
     cexpr
    ) toks

(* cexpr: conditional expressions "if bexpr then cexpr else cexpr", where
 * the condition must be a bexpr and the then and else branches are 
 * (arithmetic) conditionals. Subsumes expr. *)
(* cexpr : expr parser *)
and cexpr toks = 
    (chain (isToken T.IF) (fn _ =>
     chain rexpr (fn r =>
      chain (isToken T.THEN) (fn _ =>
       chain aexpr (fn e1 =>
        chain (isToken T.ELSE) (fn _ =>
         chain aexpr (fn e2 => 
          return (If(r,e1,e2))))))))
          +++
          aexpr)
          toks

(* decl: parses a simple or function declaration *)
val decl : decl parser =
    chain (isToken T.LET) (fn _ =>
     chain variable (fn v =>
      chain (isToken T.EQ) (fn _ =>
       chain bexpr (fn e =>
        return (Let(v,e))))))
    +++
    chain (isToken T.FUN) (fn _ =>
     chain variable (fn fname =>
      chain variable (fn x =>
       chain (isToken T.EQ) (fn _ =>
        chain bexpr (fn body =>
         return (Fun(fname, x, body)))))))
      
    (* function declaration *)

(* statement: a declaration or an expression, which can be arithmetic or boolean *)
val stmt : stmt parser =
    chain decl (fn d =>
     return (Decl d))
    +++
    chain bexpr (fn e =>
     return (Expr e))


end (* structure Parser *)

(* Notes

This defines a parser for a fairly ambitious version of the calculator language
that includes boolean expressions, boolean-valued variables, and functions that
can be applied to and can return boolean as well as integer values.

Note that the distinction between aexprs (arithmetic expressions) and
bexprs (boolean expressions) is not really about the type of the values
returned, but about the binding power of the operators involved.  Boolean
operators like || and && bind more loosely than arithmetic operators,
and arithmetic operators are subdivided into additive and multiplicative
groups, with the multiplicative operators having higher precedence.  Function
application, "f(arg)", binds more tightly than any binary operator.

The top-level expression parsing function is

    bexpr : expr parser

This encompases conditional (cexpr) and arithmetic (aexpr) expressions.

Given a list of tokens as input, these parser functions will consume as large
a prefix as can be parsed into the respective grammar phrase, but they are not
guarantee to consume all the tokens.  So in a given input line, there may remain
some unparsed "junk" when the parsing function (e.g. stmt) returns. One could
detect this situation and report an error, but we don't bother in this version.

This functional, monadic style of parser is quite inefficient, but is fairly
straightforward to implement, despite requiring a fair amount of fiddling to
get it right for any nontrivial grammar.  For a "serious" language, we would
use a more powerful and robust parsing tool, such as ml-yacc.  Similarly, for
the tokenization function (lexical analysis), there are sophisticated tools
like ml-lex and lexgen.  These are the kinds of tools used in real language
implementations like SML/NJ.

*)
