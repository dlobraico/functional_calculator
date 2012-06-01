(* eval.sml *)

(* evaluation of abstract syntax expressions, declarations, and statements in an
 * environment *)

structure Eval = 
struct

open Exprs
open Env

(* These are the values computed by eval and bound in environments.
 * The three variants of this datatype determine the "dynamically
 * checkable" types. Dynamically, all functions are of the same type. *)
datatype value
  = IntVal of int               (* integers *)
  | BoolVal of bool             (* booleans *)
  | FunVal of funExpr * venv    (* function closure *)

(* venv: value environments *)
withtype venv = value env

(* note the use of a "withtype" clause to allow a type abbreviation
 * to be associated with a datatype declaration.  The value datatype
 * indirectly self-recursive via the venv abbreviation. *)

(* "values" of statements *)
datatype stmtValue
  = ExprVal of value
  | DeclVal of variable * value

(* the following evaluation functions do "dynamic type checking" by
 * examining the intermediate values and checking that they have the
 * expected type (i.e. int, bool, or function) *)

(* EvalTypeError is raised with a dynamic type check fails *)
exception EvalTypeError

(* BadOp raised when a unary operator is applied as a binary op,
 * or vice versa *)
exception BadOp

(* applyUnOp : operator * value -> value *)
fun applyUnOp (oper, v) = 
    (* deals with ~ and not, checking types of arguments *)
    (case oper
       of Neg =>
	    (case v
	      of IntVal v1 => IntVal (~v1)
	       | _ => raise EvalTypeError)
        | Not  =>
	    (case v
	      of BoolVal v1 => BoolVal (not v1)
	       | _ => raise EvalTypeError)
        | _ => raise BadOp)

fun applyBinOp (oper, e1, e2, env) =
    let fun checkIntArgs (opr : int * int -> 'a) : 'a = 
      (* evaluate e1 and e2 and check that results are both
             IntVal, then apply the opr, yielding either int or bool
             depending on the opr *)
      case (eval(e1, env), eval(e2, env)) of
           (IntVal v1, IntVal v2) => (opr(v1,v2))
         | (_, _) => raise EvalTypeError
    in case oper
	 of     Plus => IntVal(checkIntArgs (op +))
          | Minus => IntVal(checkIntArgs (op -))
          | Times => IntVal(checkIntArgs (op * ))
          | Div => IntVal(checkIntArgs (op div))
          | Mod => IntVal(checkIntArgs (op mod))
          | Equal => BoolVal(checkIntArgs (op =))
          | NotEqual => BoolVal(checkIntArgs (op <>))
          | Less => BoolVal(checkIntArgs (op <))
          | Greater => BoolVal(checkIntArgs (op >))
          | LessEq => BoolVal(checkIntArgs (op <=))
          | GreaterEq => BoolVal(checkIntArgs (op >=))
         (* other opers, arithmetic and relational *)
         (* we implement short-circuit evaluation of And and Or *)
    end (* fun applyBinOp *)

(* eval : expr * venv -> int *)
and eval (e, env) =
    case e
      of Nat n => IntVal n
       | True => (BoolVal(true))
       | False => (BoolVal(false))
       | Id v => lookup (v, env)
       | UnaryApp(oper, e1) => applyUnOp(oper,(eval(e1,env))) (* use applyUnOp *)
       | BinaryApp(And, e1, e2) => (case eval(e1, env) of 
                                        BoolVal(true) => eval(e2, env) 
                                      | _ => BoolVal(false))
       | BinaryApp(Or, e1, e2) => (case eval(e1, env) of 
                                        BoolVal(false) => eval(e2, env) 
                                      | _ => BoolVal(true))
       | BinaryApp(oper, e1, e2) => applyBinOp(oper,e1,e2,env) (* use applyBinOp *)
       | If (e1,e2,e3) => 
           (* we could check that the values of e2 and e3 have
            * the same dynamic type, but we don't *)
            (case (eval(e1, env)) of
                 BoolVal b => if b then (eval(e2,env)) else (eval(e3,env))
               | _ => raise EvalTypeError)
       | App (f, arg) =>  (* this handles recursive functions *)
	   (case lookup(f,env)
             of funval as FunVal(Fn(p,body), envClosure) =>
		  let val argval = eval(arg, env)
		      val env' = bind(f, funval, bind(p, argval, envClosure))
		  in eval(body, env')
		  end
	      | _ => raise EvalTypeError)


(* evalDecl : venv -> decl -> stmtValue * venv *)
fun evalDecl env (Let(v,e)) = (DeclVal(v,(eval(e, env))), (v,(eval(e,env)))::env)
  | evalDecl env (Fun(f,x,e)) = 
    let val fval = FunVal(Fn(x,e), env)
     in (DeclVal(f,fval), bind(f, fval, env))
    end

(* evalStmt : env -> stmt -> (stmtValue * env) *)
fun evalStmt env (Decl d) = evalDecl env d
  | evalStmt env (Expr e) = (ExprVal(eval(e, env)), env)

end (* structure Eval *)


(* Notes

This evaluator is fairly typical of a simple interpreter.  It takes
an expression (or declaration/statement) and an environment mapping
variables (strings) to values.  The environment is needed to interpret
free variables that occur in expressions.

In this simple calculator language, variables are only bound at the
"top level" -- there are no local binding mechanisms except for the
parameter variable binding in function declarations.

The eval functions do dynamic type checking, making sure that the
values passed to primitive functions have the right types, and raising
an exception in case they do not.  This sort of dynamic checking is
not available for applications of user-definded functions.  A dynamic
type error aborts the evaluation by raising the EvalTypeError exception.
With more effort, more detailed error reporting could be implemented
(e.g. reporting which operator was being applied, and what the erroneous
argument type 

A static type checker could eliminate the possibility of these dynamically
checked type errors, and then we could simplify the evaluator by removing
the checks.

The evaluator supports recursive functions.  For instance, the factorial
function is defined by

    fun f x = if x == 0 then 1 else x * f(x-1)

Recursive calls are supported in the "App (f, arg)" case of eval by adding
the function binding, as well as the parameter variable binding, to the
closure environment when evaluating the function body.

*)
