(* calc.sml *)

structure Calc =
struct

  (* these are the "imported" modules *)
  structure S = ListInstream
  structure R = Reader
  structure T = Tokens
  structure E = Exprs
  structure P = Parser
  structure V = Eval

  (* printing values and "statement values" *)

  (* printVal : V.value -> unit *)
  fun printVal (V.IntVal n) =
      print (Int.toString n)
    | printVal (V.BoolVal b) =
      print (Bool.toString b)
    | printVal (V.FunVal _) =
      print ("<function>")

  (* printStmt : V.stmtValue -> unit *)
  fun printStmt (V.ExprVal v) =
      (print ">> "; printVal v; print "\n")
    | printStmt (V.DeclVal(var,v)) =
      (print (">> "^var^" = ");
       printVal v; print "\n")


  (* repl : Env.env -> unit
   * The calculator's read-eval-print loop.  It parses one input line at
   * a time. *)
  fun repl (env) = (print "# ";  (* prompt string *)
      (* input one line *)
      case TextIO.inputLine TextIO.stdIn
        of NONE => (print "exit\n")  (* no input, exit *)
         | SOME line =>
           (* tokenize the line *)
           (case (T.tokenize(S.fromString line))
	      of NONE => 
                 (print "Error - tokenize\n";
		  repl env)
	       | SOME (toks,_) =>
                 (* parse token stream as a statement (expr. or decl.) *)
	         (case (P.stmt toks)
	            of NONE =>
                       (print ("Error - parse failure: "^line^"\n");
			repl env)
	             | SOME(s,_) =>
                       (* parsed one statement, evaluate it *)
                       (let val (res,env') = V.evalStmt env s
                         in printStmt res;
			    repl env'
		        end
			handle Env.Unbound s => (* handling unbound variable errors *)
                          (print ("Error - unbound var: "^s^"\n");
			   repl env)))))

  (* calc: unit -> unit
   * The top-level calculator entry point. *)
  fun calc () = repl (Env.empty)

end (* structure Calc *)

(* Notes

The REPL could try to parse multiple (semicolon-separated) statements per line,
but to simplify things a bit we are only processing one statement per line
by calling P.stmt on the tokenized line.

*)
