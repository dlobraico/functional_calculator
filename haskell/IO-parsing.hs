-- IO-parsing.hs
-- Simple example of IO input being parsed by a Reader

{- This code illustrates the typical pattern of how a parser
   would be driven as a client of a top-level IO action
   (run in this case). The IO action is responsible for doing
   actual input, and it decides when it has collected enough
   input to invoke the pure function (process in this case)
   that processes the input it has gathered.

   This example also provides an answer for Problem 4
   of Homework 6. Pure functions (like Readers, for instance)
   have to be invoked from IO actions.  "Using" an IO action
   like getChar in the body of a pure function does not work
   because the action will never be performed.
-}

{- getInput gathers input for processing by expr, which
   parses an Instream = [Char] = String.
   This version gathers characters until it encounters
   the first newline character (i.e. it inputs one line),
   but to make things intereresting it adds a simple
   input "editing" feature, which interprets the character
   'D' as a kind of backspace that causes the previous
   character to be deleted.

   When getInput has collected a whole line of input,
   terminated by newline, it passes the collected input
   (the variable cs) to the process function, which 
   uses the Reader expr to parse the input as a simple
   additive expression and then evaluate that expression.
-}

-- Parser for expressions:
--   expr :: Reader Int
-- is defined in Reader

import Reader

getInput :: String -> IO (Maybe Int)
getInput cs
  =  do c <- getChar
        if c == '\n' then        -- end of input chunk
           return (process cs)
         else if c == 'D' then     -- input editing
                getInput (init cs)    -- delete last char
               else getInput (cs++[c])

process :: String -> Maybe Int
process cs =
  case performRdr expr cs of
    Nothing -> Nothing
    Just (n, s) -> Just n

run :: IO ()
run = do x <-getInput ""
         case x of {Nothing -> print "?"; Just n -> print n}


{-
The input editing (D for backspace) functionality of getInput
is not very realistic.  Normally when a program is getting
interactive input, say from stdin connected to a terminal,
the terminal program is responsible for inputing characters,
echoing them to the terminal window, and handling input
editing functions like backspace or delete. There is often
a way to put a terminal in "raw" mode where the terminal just
passes characters to the program without any processing, but
then the program assumes complete responsibility, including
for echoing characters that are typed.

If we leave out the special editing function of 'D', we may
as well use getLine in place of getChar, and input a whole
line at a time.  Then the getInput function becomes a bit
simpler:

getInput cs
  = do cs <- getLine
       return (process cs)

-}
