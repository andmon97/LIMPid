-- imports
import Prelude hiding ((+++))
import Data.Char 
import System.IO 

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



--                  PARSER FUNCTIONS                --

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