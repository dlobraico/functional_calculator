module Parser where
import Tokens
import Exprs

data Parser a = P (TokenStream -> Maybe (a, TokenStream))

instance Monad Parser where
    P p >>= f = P $ 
        \toks -> case p toks of
                    Just (r, toks') -> parse (f r) toks'
                    Nothing -> Nothing
    return v = P $ \toks -> Just (v, toks)
    fail _ = P $ \_ -> Nothing

choice :: Parser a -> Parser a -> Parser a
choice (P p) (P q) = P (\toks ->
                            case p toks of 
                                Nothing -> q toks
                                res -> res)

(+++) :: Parser a -> Parser a -> Parser a
(+++) = choice

getToken :: Parser Token 
getToken = P (\toks -> 
                case toks of 
                    [] -> Nothing
                    (t:ts) -> Just (t, ts))

parse :: Parser a -> TokenStream -> Maybe(a, TokenStream)
parse (P p) = p

isToken :: Token -> Parser ()
isToken t = getToken >>= \t' -> if t == t' then return () else fail "Nonmatching"

natExpr :: Parser Expr
natExpr = getToken >>= \n -> (case n of
                                NAT x -> return (Nat x)
                                _ -> fail "Nonmatching")

identExpr :: Parser Expr
identExpr = getToken >>= \n -> (case n of
                                ID i -> return (Id i)
                                _ -> fail "Nonmatching")

aexpr :: Parser Expr
aexpr = do t <- aterm
           do isToken PLUS
              e <- aexpr
              return (BinaryApp Plus t e)
            +++
            do isToken MINUS
               e <- aexpr
               return (BinaryApp Minus t e)
             +++
             return t

aterm :: Parser Expr
aterm = do s <- afactor
           do isToken TIMES
              t <- aterm
              return (BinaryApp Times s t)
            +++
            do isToken DIV
               t <- aterm
               return (BinaryApp Div s t)
             +++
             do isToken MOD
                t <- aterm
                return (BinaryApp Mod s t)
              +++
              return s

funApp :: Parser Expr
funApp = do func <- variable
            isToken LPAR
            e <- aexpr
            isToken RPAR
            return (App func e)

afactor :: Parser Expr
afactor = do isToken NEG
             a <- afactor
             return (UnaryApp Neg a)
          +++
          do isToken LPAR
             e <- aexpr
             isToken RPAR
             return e
          +++
          funApp
          +++
          natExpr
          +++
          identExpr

rexpr :: Parser Expr
rexpr = do e1 <- aexpr
           do isToken EQUAL 
              e2 <- aexpr
              return (BinaryApp Equal e1 e2)
            +++
            do isToken NOTEQUAL
               e2 <- aexpr
               return (BinaryApp NotEqual e1 e2)
             +++
            do isToken LESS
               e2 <- aexpr
               return (BinaryApp Less e1 e2)
             +++
            do isToken GREATER
               e2 <- aexpr
               return (BinaryApp Greater e1 e2)
             +++
            do isToken LESSEQ
               e2 <- aexpr
               return (BinaryApp LessEq e1 e2)
             +++
            do isToken GREATEREQ
               e2 <- aexpr
               return (BinaryApp GreaterEq e1 e2)

bexpr :: Parser Expr
bexpr = do b1 <- bterm
           do isToken OR
              b2 <- bexpr
              return (BinaryApp Or b1 b2)
            +++
            return b1

bterm :: Parser Expr
bterm = do b1 <- bfactor
           do isToken AND
              b2  <- bexpr
              return (BinaryApp And b1 b2)
            +++
            return b1

bfactor :: Parser Expr
bfactor = rexpr +++ cexpr

cexpr :: Parser Expr
cexpr = do isToken IF
           r <- rexpr
           isToken THEN
           e1 <- aexpr
           isToken ELSE
           e2 <- aexpr
           return (If r e1 e2)
        +++
        aexpr

variable :: Parser String
variable = getToken >>= \t -> (case t of 
                                    ID s -> return s
                                    _ -> fail "Nonmatching")

 
decl :: Parser Decl
decl = do isToken LET
          var <- variable
          isToken Tokens.EQ
          e <- bexpr
          return (Let var e)
       +++
       do isToken FUN
          f <- variable
          x <- variable
          isToken Tokens.EQ
          e <- bexpr
          return (Fun f x e)

stmt :: Parser Stmt
stmt = do d <- decl
          return $ D d
          +++
          do e <- bexpr
             return $ E e

-- Using a REPL instead of the below
-- prog :: Parser Prog
-- prog = do s <- stmt
--           do isToken SEMI
--              do p <- prog
--                 return (s:p)
--                 +++
--                 return [s]
          
