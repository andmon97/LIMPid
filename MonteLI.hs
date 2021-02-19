-- imports
import Prelude hiding ((+++))
import Data.Char 
import System.IO 

getCh           :: IO Char
getCh           = do hSetEcho stdin False
                     c <- getChar
                     hSetEcho stdin True
                     return c

                     
-- Definition of the environment: couple with name and value of the variable
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