Hello! I have made a valiant attempt at creating both Haskell and SML variants
of the simple calculator language given in the assignment. 

I have been successful (I think) at least in implementing:

    * basic arithmetic operators (+ - * / %)
    * negation (e.g. ~1)
    * declarations (let x = 10)
    * function declarations (fun f x = 10*x), including recursive declarations 
        like factorial
    * shortcircuited boolean operators (&&, ||)
    * conditionals (if x then y else z)
    * relational operators (<, <=, >, >=, ==, /=)

The layout of both applications follows the assignment closely. 
    * Calc.hs/calc.sml : the basic REPL
    * Env.hs/env.sml : implementation of an association-list for storing environments
    * Eval.hs/Eval.sml : evaluation of expressions
    * Exprs.hs/exprs.sml : definiton of expressions
    * Reader.hs/listinstream.sml, reader.sml, readerfn.sml : implementation of stream type and reader
    * Tokens.hs/tokens.sml : implementation of tokenizer to convert strings to tokens
    * Parser.hs/parser.sml : implementation of parser to convert tokens to abstract expressions
    * Test.hs/test.sml : test suite


I have also included a test suite for each version that confirms the correct
output for each of the following expressions:

1 + 1 = 2
1 - 1 = 0
10 + 2 - 1 = 11
4 * 9 = 36
4/2 = 2
5*2+3 = 13
5*2+3/3-1 = 10
((2*6)+3)/(6-1) = 3
~8+1 = (~7)
((~4*7) - 2)/10 = (~3)

let x = 10
10 * x = 100

fun f x = 8 * x + 32
f(10) = 112

let y = 7
let f x = 8 * x + 32 - y
f(10) = 105
let z = 10
f(z) = 105

if 1 < 5 then 10 else 7 = 10
if 1 > 5 then 10 else 7 = 7
if 1 /= 5 then 190 else 7 = 190
if 5 == 5 then 83 else 7 = 83
if 5 <= 5 then 83 else 7 = 83
if 5 >= 5 then 83 else 7 = 83
if 4 <= 5 then 83 else 7 = 83
if 4 >= 5 then 83 else 7 = 7

fun fact n = if n == 0 then 1 else n*fact(n-1)
fact(10) = 3628800

1 > 2 || 2 < 3 = true
1 > 2 && 2 < 3 = false
1 < 2 || 2 < 3 = true
1 < 2 && 2 < 3 = true

===========
= HASKELL =
===========

To start the calculator, simply open `ghci` from the `haskell` directory and run
`:load Calc.hs`. Then run, `Calc.calc` to start the interactive calculator. 

To run the test suite, run `:load Test.hs`. Then run `runTestTT tests`. If
everything is working correctly, you should see output like the following:

    Cases: 16  Tried: 16  Errors: 0  Failures: 0
    Counts {cases = 16, tried = 16, errors = 0, failures = 0}

=======
= SML =
=======

To start the calculator, run `CM.make "calc.cm";` from within the SMLNJ
interpreter. Then run `Calc.calc ();` to start the interactive interpreter. 

To run the test suite, run `:use test.sml;` then run `Test.runTests`. If the
output looks like 

    val it = [] : (string * bool) list

then the tests have all passed successfully. Otherwise, the name and number of
the failing test along with the boolean value "false" will be displayed in the
result list for each failure. 
