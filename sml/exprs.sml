(* exprs.sml *)

structure Exprs =
struct

  type variable = string

  (* a general operator type, including arithmetic, relational, boolean operators *)
  datatype operator 
    = Plus | Minus | Times | Div | Mod | Neg    (* arithmetic *)
    | Equal | NotEqual | Less | Greater | LessEq | GreaterEq  (* relational *)
    | True | False | And | Or | Not   (* boolean *)

  (* a general expression type, covering arithmetic, relational, and boolean expressions *)
  datatype expr
    = Nat of int       (* number constants (all nonnegative) *)
    | True | False     (* boolean constants *)
    | Id of variable   (* variables (which should be bound in environment) *)

    (* application of primitive unary and binary operators *)
    | UnaryApp of operator * expr
    | BinaryApp of operator * expr * expr

    | If of expr * expr * expr  (* conditional *)
    | App of variable * expr    (* application of defined function *)

  (* function expressions -- equivalent to a lambda expression 
   * these are not directly represented in the calclulator syntax,
   * but are used in the representation of function values (see Eval.FunVal) *)
  datatype funExpr = Fn of variable * expr

  (* declarations: simple variable declarations and function declarations *)
  datatype decl
    = Let of variable * expr
    | Fun of variable * variable * expr  (* fun f x = expr ==> Fun(f,x,expr) *)

  (* statements: either an expression, or a declaration *)
  datatype stmt
    = Decl of decl
    | Expr of expr
   
  (* prog: a sequence of statements, with nested scopes. I.e. the scope of 
   * a declaration in the sequence is the remainder of the sequence. *)
  type prog = stmt list

end (* structure Exprs *)
