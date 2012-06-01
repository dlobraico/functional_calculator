-- Calc.hs
module Calc where

import Reader
import Tokens
import Parser
import Eval


printVal :: Value -> IO ()
printVal (IntVal n) = print n
printVal (BoolVal b) = print b
printVal (FunVal _ _) = print "<function>"

printStmt :: StmtValue -> IO ()
printStmt (ExprVal v) = do putStr ">> "
                           printVal v
printStmt (DeclVal var v) = do putStr ">> "
                               putStr var
                               putStr " = "
                               printVal v

repl :: VEnv -> IO ()
repl env = do
        putStr "# "
        instr <- getLine
        case performRdr tokenize instr of
            Nothing -> do putStrLn "Error - tokenizer"
                          repl env
            Just (toks, _) ->
                case parse stmt toks of
                    Nothing -> do putStrLn "Error - parser"
                                  repl env
                    Just (v, _) -> do let (res, env') = evalStmt v env
                                      printStmt res 
                                      repl env'

calc :: IO ()
calc = repl []
