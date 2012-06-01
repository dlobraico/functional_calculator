-- Purely Functional input -- Reader monad
module Reader where

import Char

type Instream = [Char]

-- define Reader as a datatype rather than as an abbreviation
-- so we can define a Monad instance for it
data Reader a = R (Instream -> Maybe (a, Instream))

-- Reader as a monad
instance Monad Reader where
  return v = R (\ instream -> Just (v,instream))
  R p >>= f = R (\ instream ->
                   case p instream of
	             Nothing -> Nothing
	             Just (v,instream') ->
                       let R q = f v in q instream')
  fail s = R (\ instream -> Nothing) 

inputc :: Reader Char
inputc = R (\ inp -> 
             case inp of
               [] -> Nothing
               (c:cs) -> Just (c, cs))

performRdr :: Reader a -> (Instream -> Maybe (a, Instream))
performRdr (R p) = p

choice :: Reader a -> Reader a -> Reader a
choice (R p) (R q) =
  R (\instream ->
       case p instream of
         Nothing -> q instream
         res -> res)

-- infix operator for choice
(+++) = choice

-- A "reader transform" that corresponds to the Kleene star.
star :: Reader a -> Reader [a]
star r = starPlus r +++ return []

starPlus :: Reader a -> Reader [a]
starPlus r = r >>= (\v -> star r >>= (\ vs -> return (v:vs)))

-- match a character satisfying a character property
sat :: (Char -> Bool) -> Reader Char
sat prop = inputc >>= (\c -> if prop c then return c else fail "")

-- match a designated character
checkChar :: Char -> Reader Char
checkChar c = sat (\ c' -> c == c')

-- match a designated string
checkString :: [Char] -> Reader [Char]
checkString [] = return []
checkString (s @ (c:cs)) = 
   checkChar c >>= (\ _  -> checkString cs >>= (\ _ -> (return s)))

alphaRdr :: Reader Char
alphaRdr = sat isAlpha
alphaNumRdr :: Reader Char
alphaNumRdr = sat isAlphaNum
digitRdr :: Reader Char
digitRdr = sat isDigit
spaceRdr :: Reader Char
spaceRdr = sat isSpace
nonSpaceRdr :: Reader Char
nonSpaceRdr = sat (not . isSpace)

-- ident -- alphanumeric identifiers
ident :: Reader String
ident = alphaRdr >>= (\ c ->
         star alphaNumRdr >>= (\ cs ->
          return (c:cs)))

nat :: Reader Int
nat = starPlus digitRdr >>= (\ cs ->
       return(read cs))

space :: Reader ()
space = star spaceRdr >>= (\ _ ->
         return ())

-- tokenizing -- space separated tokens
token :: Reader a -> Reader a
token r =
    space >>= (\ () -> 
     r >>= (\ v -> 
      space >>= (\ () ->
       return v)))

identifier :: Reader String
identifier = token ident

natural :: Reader Int
natural = token nat

symbol :: String -> Reader String
symbol s = token(checkString s)

invert :: a -> Reader c -> Reader a
invert v (R r) = R (\instream -> case r instream of
                                            Nothing -> Just (v, instream)
                                            _ -> Nothing)

keyword :: String -> Reader String
keyword s = token (checkString s) -- >>= \v -> invert v alphaRdr

{- Examples

-- example: scan to first space, collecting string
-- was scan1 in input.sml
scanToSpace :: Reader String
scanToSpace = star nonSpaceRdr

-- here is a function to input a list of natural numbers in
-- the format "[n1, n2, ... , nk]"

natList :: Reader [Int]
natList =
   checkChar '[' >>= (\ _ ->
    natural >>= (\ n ->
      (star (checkChar ',' >>= (\ _ ->
                    natural))) >>= (\ ns ->
       checkChar ']' >>= (\ _ ->
        return (n:ns)))))

-- using do notation, natlist would look like this:
natList =
   do checkChar '['
      n <- natural
      ns <- star (do checkChar ','
                     natural)
      checkChar ']'
      return (n:ns)
-- parse and evaluat simple additive expressions formed with nats and plus
expr :: Reader Int
expr = do n1 <- nat
          do symbol "+"
             n2 <- expr
             return (n1+n2)
            +++ return n1
-}
