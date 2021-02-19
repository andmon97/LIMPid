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