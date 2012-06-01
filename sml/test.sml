structure Test =
struct
    structure S = ListInstream
    structure R = Reader
    structure T = Tokens
    structure E = Exprs
    structure P = Parser
    structure V = Eval
    
    
  fun testCalc (instr,env) = 
           (case (T.tokenize(S.fromString instr))
              of NONE => NONE
               | SOME (toks,_) =>
                   (case (P.stmt toks)
                      of NONE => NONE
                       | SOME(s,_) => SOME (V.evalStmt env s)))


  fun confirmIntResult (instr, intendedResult, initEnv) =
    (case (testCalc(instr, initEnv)) of
         SOME (V.ExprVal (V.IntVal v), resultEnv) => if v = intendedResult 
                                                     then true else false
       | _ => false)

  fun confirmBoolResult (instr, intendedResult, initEnv) =
    (case (testCalc(instr, initEnv)) of
         SOME (V.ExprVal (V.BoolVal b), resultEnv) => if b = intendedResult 
                                                     then true else false
       | _ => false)

  fun getNewEnv (instr, initEnv) =
    (case (testCalc(instr, initEnv)) of
          SOME (_, resultEnv) => resultEnv
        | NONE => Env.empty)
       
  val tests = [
    ("1: simple addition", confirmIntResult("1 + 1", 2, Env.empty))
  , ("2: simple subtraction", confirmIntResult("1 - 1", 0, Env.empty))
  , ("3: addition and subtraction", confirmIntResult("10 + 2 - 1", 11, Env.empty))
  , ("4: simple multiplication", confirmIntResult("4 * 9", 36, Env.empty))
  , ("5: simple division", confirmIntResult("4/2", 2, Env.empty))
  , ("6: multiplication and addition", confirmIntResult("5*2+3", 13, Env.empty))
  , ("7: simple arithmetic with order of operations", confirmIntResult("5*2+3/3-1", 10, Env.empty))
  , ("8: arithmetic with parentheses and order of operations", confirmIntResult("((2*6)+3)/(6-1)", 3, Env.empty))
  , ("9: addition with negation", confirmIntResult("~8+1", (~7), Env.empty))
  , ("10: arithmetic with negation and parentheses", confirmIntResult("((~4*7) - 2)/10", (~3), Env.empty))
  , ("11: simple declaration", confirmIntResult("10 * x", 100, (getNewEnv("let x = 10", Env.empty))))
  , ("12: simple function declaration", confirmIntResult("f(10)", 112, (getNewEnv("fun f x = 8 * x + 32", Env.empty))))
  , ("13: function declaration with variable declaration", confirmIntResult("f(10)", 105, (getNewEnv("fun f x = 8*x+32-y", getNewEnv("let y = 7", Env.empty)))))
  , ("14: function declaration with variable in declaration and for parameter", confirmIntResult("f(z)", 105, (getNewEnv("fun f x = 8*x+32-y", getNewEnv("let y = 7", getNewEnv("let z = 10", Env.empty))))))
  , ("15: simple conditional", confirmIntResult("if 1 < 5 then 10 else 7", 10, Env.empty))
  , ("16: simple conditional", confirmIntResult("if 1 > 5 then 10 else 7", 7, Env.empty))
  , ("17: simple conditional", confirmIntResult("if 1 /= 5 then 190 else 7", 190, Env.empty))
  , ("18: simple conditional", confirmIntResult("if 5 == 5 then 83 else 7", 83, Env.empty))
  , ("19: simple conditional", confirmIntResult("if 5 <= 5 then 83 else 7", 83, Env.empty))
  , ("20: simple conditional", confirmIntResult("if 5 >= 5 then 83 else 7", 83, Env.empty))
  , ("21: simple conditional", confirmIntResult("if 4 <= 5 then 83 else 7", 83, Env.empty))
  , ("22: simple conditional", confirmIntResult("if 4 >= 5 then 83 else 7", 7, Env.empty))
  , ("23: recursive function declaration", confirmIntResult("fact(10)", 3628800, (getNewEnv("fun fact n = if n == 0 then 1 else n*fact(n-1)", Env.empty))))
  , ("24: simple boolean operators", confirmBoolResult("1 > 2 || 2 < 3", true, Env.empty))
  , ("25: simple boolean operators", confirmBoolResult("1 > 2 && 2 < 3", false, Env.empty))
  , ("26: simple boolean operators", confirmBoolResult("1 < 2 || 2 < 3", true, Env.empty))
  , ("27: simple boolean operators", confirmBoolResult("1 < 2 && 2 < 3", true, Env.empty))
              ]

  fun runTest (name, result) = if result then false else true

  val runTests = List.filter runTest tests

end
