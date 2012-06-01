module Eval where
import Exprs
import Env

type VEnv = Env Value
data Value = IntVal Int | BoolVal Bool | FunVal FunExpr VEnv
    deriving (Show, Eq)

data StmtValue = ExprVal Value | DeclVal Var Value
            deriving (Show, Eq)

applyUnOp :: Operator -> Value -> Value
applyUnOp Neg (IntVal v) = IntVal (-v)
applyUnOp Neg _ = error "EvalTypeError"
applyUnOp Not (BoolVal v) = BoolVal (not v)
applyUnOp Not _ = error "EvalTypeError"
applyUnOp _   _ = error "BadOp"

applyBinOp :: Operator -> Expr -> Expr -> VEnv -> Value
applyBinOp oper e1 e2 env = 
            let checkIntArgs opr = case (eval e1 env, eval e2 env) of
                                    (IntVal v1, IntVal v2) -> (v1 `opr` v2)
                                    (_, _) -> error "EvalTypeError"
            in case oper of
                Plus       -> IntVal  (checkIntArgs (+))
                Minus      -> IntVal  (checkIntArgs (-))
                Times      -> IntVal  (checkIntArgs (*))
                Div        -> IntVal  (checkIntArgs div)
                Mod        -> IntVal  (checkIntArgs mod)
                Equal      -> BoolVal (checkIntArgs (==))
                NotEqual   -> BoolVal (checkIntArgs (/=))
                Less       -> BoolVal (checkIntArgs (<))
                Greater    -> BoolVal (checkIntArgs (>))
                LessEq     -> BoolVal (checkIntArgs (<=))
                GreaterEq  -> BoolVal (checkIntArgs (>=))

eval :: Expr -> VEnv -> Value
eval (Nat n) _  = IntVal n
eval (Id i) env  = Env.lookup i env
eval (UnaryApp op e1) env = applyUnOp op (eval e1 env)
eval (BinaryApp And e1 e2) env = case eval e1 env of 
                                    BoolVal Prelude.True -> eval e2 env
                                    _ -> BoolVal Prelude.False
eval (BinaryApp Or e1 e2) env = case eval e1 env of
                                    BoolVal Prelude.False -> eval e2 env
                                    _ -> BoolVal Prelude.True
eval (BinaryApp op e1 e2) env = applyBinOp op e1 e2 env
eval (If r e1 e2) env = case eval r env of 
                            BoolVal b -> if b then eval e1 env else eval e2 env
                            _ -> error "EvalTypeError"
eval (App f arg) env = 
            case Env.lookup f env of
                funval@(FunVal (Fn var body) envClosure) ->
                    let argval = eval arg env
                        env' = bind f funval (bind var argval envClosure)
                    in eval body env'
                _ -> error "Eval type error"

evalDecl :: Decl -> VEnv -> (StmtValue, VEnv)
evalDecl (Let v e) env = (DeclVal v (eval e env), (v,(eval e env)):env)
evalDecl (Fun f x e) env = 
    let fval = (FunVal (Fn x e) env)
    in (DeclVal f fval, bind f fval env)

evalStmt :: Stmt -> VEnv -> (StmtValue, VEnv)
evalStmt (D d) env = evalDecl d env
evalStmt (E e) env = (ExprVal (eval e env), env)
