-- imports
import Prelude hiding ((+++))
import Data.Char 
import System.IO 
import Data.List (isPrefixOf)

getCh           :: IO Char
getCh           = do hSetEcho stdin False
                     c <- getChar
                     hSetEcho stdin True
                     return c


-- Definition of the environment: couple with name and value of the variables
type Env    =   [(String, String)]

-- Definition of the Parser: in input there are the Environment and the input string, and in output a triple
-- wich contains the Envoingment, the elaboration of the input and the rest of the input.
type Parser a = Env -> String -> [(Env, a, String)]


-- _____________________________________________________________________________________________________________________________________--
--                  PARSER FUNCTIONS                --
-- _____________________________________________________________________________________________________________________________________--
-- "item" parses the first element of a list and return the rest of the list
item            :: Parser Char
item            = \env inp -> case inp of
                            [] -> []
                            (x:xs) -> [(env, x, xs)]

-- "parseReturn" modify the environment after an assignment
-- return a Parser after sostituting the elements of the expression a and Env of the triple with the input values
parseReturn          :: Env -> a -> Parser a
parseReturn newenv v = \env inp -> [(newenv, v, inp)]

-- "failure" stops the parsing
failure         :: Parser a
failure         = \env inp -> []

-- "parse" executes the parsing
parse           :: Parser a -> Env -> String ->[(Env, a, String)]
parse p env inp = p env inp

-- SEQUENCING operator (>>>=). parse p >>>= f fails if the application of the parser p to the input string fails, and otherwise applies the function f to the result value to give a
-- second parser, which is then applied to the output string to give the final result
(>>>=)          :: Parser a -> (Env -> a -> Parser b) -> Parser b
p >>>= f        = \env inp -> case parse p env inp of 
                                [] -> []
                                [(env, v, out)] -> parse (f env v) env out

-- CHOICE operator (+++). p +++ q execute p, otherwise if p fails executes q. Input and env are transferred from a parser to the other one
(+++)           :: Parser a -> Parser a -> Parser a
p +++ q         = \env inp -> case p env inp of
                                [] -> parse q env inp
                                [(env, v, out)] -> [(env, v, out)]

-- Parse a character if the predicate p is satisfied
sat             :: (Char -> Bool) -> Parser Char
sat p           = item >>>= \env x ->
                    if p x then
                        parseReturn env x
                    else 
                        failure

-- Parse ua specific character 
char            :: Char -> Parser Char
char x          = sat (x ==) 


-- _____________________________________________________________________________________________________________________________________--
--                  UTILITIES FUNCTIONS                     --
-- _____________________________________________________________________________________________________________________________________--
{- "setEnv" sets the environment adding or sobstituting a couple var-val
- v is the NAME of the variable
- a is the VALUE of the var (String type because it will be substituted in the string code)
- es is the environment -}
setEnv         :: String -> String -> Env -> Env 
setEnv v a []  = [(v, a)]
setEnv v a (e:es)
                | (fst e) == v  = [(v, a)] ++ es 
                | otherwise     = e: (setEnv v a es)

{- "replace" repl. a variable in the environment
- v is the couple name,value of the variable
- xs is the expression to evaluate -}
replace        :: (String, String) -> String -> String
replace v []   = []
replace v xs 
    | (fst v) `isPrefixOf` xs = (snd v) ++ replace v (drop (length (fst v)) xs)
    | otherwise = (xs !! 0) : replace v (drop 1 xs)

{- "bind" is a function that interpeters all the variable in the env
-- es enviroment of variables
-- xs expression to evaluate -}
bind           :: Env -> String -> String 
bind [] xs     = xs
bind es []     = []
bind (e:es) xs = bind es (replace e xs) 

-- "getCode" extract the code (type of a) from the tuple --
getCode             :: [(Env, a, String)] -> a 
getCode [(_,x,_)]   = x 

-- extract the env state (is like a toString method) -- 
getMemory               :: [(Env, a, String)] -> String 
getMemory []            = []
getMemory [([],_,_)]    = []

-- extract the environment from the tuple --
getEnv                  :: [(Env, a, String)] -> Env 
getEnv []               = []
getEnv [([],_,_)]       = []
getEnv [(x,_,_)]        = x 


-- _____________________________________________________________________________________________________________________________________--
--                      KEYWORDS AND SYMBOLS
-- _____________________________________________________________________________________________________________________________________--

-- Parse the "True" keyword
trueKeyword     :: Parser String
trueKeyword     = char 'T' >>>= \env _ -> char 'r' >>>= \_ _ -> char 'u' >>>= \_ _ -> char 'e' >>>= \_ _ -> parseReturn env ("True")

-- Parse the "False" keyword
falseKeyword    :: Parser String
falseKeyword    = char 'F' >>>= \env _ -> char 'a' >>>= \_ _ -> char 'l' >>>= \_ _ -> char 's' >>>= \_ _ -> char 'e' >>>= \_ _ -> parseReturn env ("False")

-- Parse the "skip" keyword
skipKeyword     :: Parser String
skipKeyword     = char 's' >>>= \env _ -> char 'k' >>>= \_ _ -> char 'i' >>>= \_ _ -> char 'p' >>>= \_ _ -> parseReturn env ("skip")

-- Parse the "if" keyword
ifKeyword       :: Parser String
ifKeyword       = char 'i' >>>= \env _ -> char 'f' >>>= \_ _ -> space >>>= \_ _ -> parseReturn env ("if ")

-- Parse the "else" keyword
elseKeyword     :: Parser String
elseKeyword     = char 'e' >>>= \env _ -> char 'l' >>>= \_ _ -> char 's' >>>= \_ _ -> char 'e' >>>= \_ _ -> space >>>= \_ _ -> parseReturn env ("else ")

-- Parse the "do" keyword
doKeyword       :: Parser String
doKeyword       = char 'd' >>>= \env _ -> char 'o' >>>= \_ _ -> space >>>= \_ _ -> parseReturn env ("do ")

-- Parse the "while" keyword
whileKeyword    :: Parser String
whileKeyword    = char 'w' >>>= \env _ -> char 'h' >>>= \_ _ -> char 'i' >>>= \_ _ -> char 'l'  >>>= \_ _ -> char 'e' >>>= \_ _ -> space >>>= \_ _ -> parseReturn env ("while ")

--      chars parsers
-- Parse the opened pargraf
openPargraf     :: Parser String 
openPargraf     = char '{' >>>= \env _ -> space >>>= \_ _ -> parseReturn env ("{ ")

-- Parse the closed pargraf
closePargraf   :: Parser String
closePargraf   = char '}' >>>= \env _ -> parseReturn env ("}")

-- Parse the opened square parenth.
openPar        :: Parser String 
openPar        = char '(' >>>= \env _ -> parseReturn env (")")

-- Parse the closed square parenth.
closePar        :: Parser String 
closePar        = char ')' >>>= \env _ -> space >>>= \_ _ -> parseReturn env (")")