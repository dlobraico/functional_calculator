module Test where
import Calc
import Reader
import Tokens
import Eval
import Parser
import Env
import Exprs

import Test.HUnit

tests :: Test
tests = test [  "simple addition" ~: do (res, env) <- (testCalc "1+1" []) 
                                        assertEqual "result" (ExprVal (IntVal 2)) res
                                        assertEqual "environment" [] env
                ,"simple subtraction" ~: do (res, env) <- (testCalc "1-1" [])
                                            assertEqual "result" (ExprVal (IntVal 0)) res
                                            assertEqual "environment" [] env
                ,"addition and subtraction" ~: do (res, env) <- (testCalc "10+2-1" [])
                                                  assertEqual "result" (ExprVal (IntVal 11)) res
                                                  assertEqual "environment" [] env
                ,"simple multiplication" ~: do (res, env) <- (testCalc "4*9" []) 
                                               assertEqual "result" (ExprVal (IntVal 36)) res
                                               assertEqual "environment" [] env
                ,"simple division" ~: do (res, env) <- (testCalc "4/2" [])
                                         assertEqual "result" (ExprVal (IntVal 2)) res
                                         assertEqual "environment" [] env
                ,"multiplication and addition" ~: do (res, env) <- (testCalc "5*2+3" [])
                                                     assertEqual "result" (ExprVal (IntVal 13)) res
                                                     assertEqual "environment" [] env
                ,"multiplication, addition, division, subtraction" ~: do (res, env) <- (testCalc "5*2+3/3-1" [])
                                                                         assertEqual "result" (ExprVal (IntVal 10)) res
                                                                         assertEqual "environment" [] env
                ,"parentheses, all basic ops" ~: do (res, env) <- (testCalc "((2*6)+3)/(6-1)" [])
                                                    assertEqual "result" (ExprVal (IntVal 3)) res
                                                    assertEqual "environment" [] env
                ,"simple negation" ~: do (res, env) <- (testCalc "(~8+1)" [])
                                         assertEqual "result" (ExprVal (IntVal (-7))) res
                                         assertEqual "environment" [] env
                ,"complex negation" ~: do (res, env) <- (testCalc "((~4*7)-2)/10" [])
                                          assertEqual "result" (ExprVal (IntVal (-3))) res
                                          assertEqual "environment" [] env
                ,"simple declaration with use in expression" ~: do (res, env) <- (testCalc "let x = 10" [])
                                                                   assertEqual "declaration result" (DeclVal "x" (IntVal 10)) res
                                                                   assertEqual "post-declaration environment" (Env.insert "x" (IntVal 10) []) env
                                                                   (res', env') <- (testCalc "10 * x" env)
                                                                   assertEqual "expression result" (ExprVal (IntVal 100)) res'
                                                                   assertEqual "post-expression environment" (Env.insert "x" (IntVal 10) []) env'
                ,"simple function declaration with use in expression" ~: do (res, env) <- (testCalc "fun f x = 8*x + 32" [])
                                                                            assertEqual "declaration result" (DeclVal "f" (FunVal (Fn "x" (BinaryApp Plus (BinaryApp Times (Nat 8) (Id "x")) (Nat 32))) [])) res
                                                                            assertEqual "post-declaration environment" (Env.insert "f" (FunVal (Fn "x" (BinaryApp Plus (BinaryApp Times (Nat 8) (Id "x")) (Nat 32))) []) []) env
                                                                            (res', env') <- (testCalc "f(10)" env)
                                                                            assertEqual "expression result" (ExprVal (IntVal 112)) res'
                                                                            assertEqual "post-expression environment" (Env.insert "f" (FunVal (Fn "x" (BinaryApp Plus (BinaryApp Times (Nat 8) (Id "x")) (Nat 32))) []) []) env
                ,"complex function declaration with use in expression and variable parameter" ~: do (res, env) <- (testCalc "let y = 7" [])
                                                                                                    assertEqual "declaration result"  (DeclVal "y" (IntVal 7)) res
                                                                                                    assertEqual "post-declaration environment" (Env.insert "y" (IntVal 7) []) env
                                                                                                    (res', env') <- (testCalc "fun f x = 8*x + 32 - y" env)
                                                                                                    assertEqual "declaration result"  (DeclVal "f" (FunVal (Fn "x" (BinaryApp Plus (BinaryApp Times (Nat 8) (Id "x")) (BinaryApp Minus (Nat 32) (Id "y")))) [("y",IntVal 7)])) res'
                                                                                                    assertEqual "post-declaration environment" (Env.insert "f" (FunVal (Fn "x" (BinaryApp Plus (BinaryApp Times (Nat 8) (Id "x")) (BinaryApp Minus (Nat 32) (Id "y")))) [("y",IntVal 7)]) env) env'
                                                                                                    (res'', env'') <- (testCalc "f(10)" env')
                                                                                                    assertEqual "expression result" (ExprVal (IntVal 105)) res''
                                                                                                    assertEqual "post-expression environment" env' env''
                                                                                                    (res''', env''') <- (testCalc "let z = 10" env'')
                                                                                                    assertEqual "expression result" (DeclVal "z" (IntVal 10)) res'''
                                                                                                    assertEqual "post-expression environment" (("z",(IntVal 10)):env'') env'''
                                                                                                   -- (res'''', env'''') <- (testCalc "f(z)" env''')
                                                                                                   -- assertEqual "expression result" (ExprVal (IntVal 105)) res''''
                ,"basic relational operators" ~: do (res, env) <- (testCalc "if 1 < 5 then 10 else 7" [])
                                                    assertEqual "result" (ExprVal (IntVal 10)) res
                                                    assertEqual "environment" [] env
                                                    (res, env) <- (testCalc "if 1 > 5 then 10 else 7" [])
                                                    assertEqual "result" (ExprVal (IntVal 7)) res
                                                    assertEqual "environment" [] env
                                                    (res, env) <- (testCalc "if 1 /= 5 then 190 else 7" [])
                                                    assertEqual "result" (ExprVal (IntVal 190)) res
                                                    assertEqual "environment" [] env
                                                    (res, env) <- (testCalc "if 5 == 5 then 83 else 7" [])
                                                    assertEqual "result" (ExprVal (IntVal 83)) res
                                                    assertEqual "environment" [] env
                                                    (res, env) <- (testCalc "if 5 <= 5 then 83 else 7" [])
                                                    assertEqual "result" (ExprVal (IntVal 83)) res
                                                    assertEqual "environment" [] env
                                                    (res, env) <- (testCalc "if 4 <= 5 then 83 else 7" [])
                                                    assertEqual "result" (ExprVal (IntVal 83)) res
                                                    assertEqual "environment" [] env
                                                    (res, env) <- (testCalc "if 4 >= 5 then 83 else 7" [])
                                                    assertEqual "result" (ExprVal (IntVal 7)) res
                                                    assertEqual "environment" [] env
                                                    (res, env) <- (testCalc "if 5 >= 5 then 83 else 7" [])
                                                    assertEqual "result" (ExprVal (IntVal 83)) res
                                                    assertEqual "environment" [] env
                ,"recursive function declaration with use in expression" ~: do (res, env) <- (testCalc "fun fact n = if n == 0 then 1 else n*fact(n-1)" [])
                                                                               assertEqual "declaration result" (DeclVal "fact" (FunVal (Fn "n" (If (BinaryApp Equal (Id "n") (Nat 0)) (Nat 1) (BinaryApp Times (Id "n") (App "fact" (BinaryApp Minus (Id "n") (Nat 1)))))) [])) res
                                                                               assertEqual "post-expression environment" [("fact",FunVal (Fn "n" (If (BinaryApp Equal (Id "n") (Nat 0)) (Nat 1) (BinaryApp Times (Id "n") (App "fact" (BinaryApp Minus (Id "n") (Nat 1)))))) [])] env
                                                                               (res', env') <- (testCalc "fact(10)" env)
                                                                               assertEqual "expression result" (ExprVal (IntVal 3628800)) res'
                                                                               assertEqual "post-expression environment" [("fact",FunVal (Fn "n" (If (BinaryApp Equal (Id "n") (Nat 0)) (Nat 1) (BinaryApp Times (Id "n") (App "fact" (BinaryApp Minus (Id "n") (Nat 1)))))) [])] env'
                ,"simple booleans" ~: do (res, env) <- (testCalc "1 < 2 && 2 < 3" [])
                                         assertEqual "result" (ExprVal (BoolVal Prelude.True)) res
                                         (res, env) <- (testCalc "1 < 2 || 2 < 3" [])
                                         assertEqual "result" (ExprVal (BoolVal Prelude.True)) res
                                         (res, env) <- (testCalc "1 > 2 && 2 < 3" [])
                                         assertEqual "result" (ExprVal (BoolVal Prelude.False)) res
                                         (res, env) <- (testCalc "1 > 2 || 2 < 3" [])
                                         assertEqual "result" (ExprVal (BoolVal Prelude.True)) res
                ]

testCalc :: Instream -> VEnv -> IO (StmtValue, VEnv)
testCalc instr env = 
    case performRdr tokenize instr of
        Nothing -> fail "Error - tokenizer"
        Just (toks, _) ->
            case parse stmt toks of 
                Nothing -> fail "Error - parser"
                Just (v, _) -> do let (res, env') = (evalStmt v env)
                                  return (res, env')

tokenizeStr :: String -> IO ()
tokenizeStr s = do
    case performRdr tokenize s of
        Nothing -> putStrLn "ERROR: could not tokenize input"
        Just (toks, _) -> print toks
