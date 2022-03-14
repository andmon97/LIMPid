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
-- wich contains the Envoironment, the elaboration of the input and the rest of the input.
type Parser a = Env -> String -> [(Env, a, String)]


-- _____________________________________________________________________________________________________________________________________--
--                  PARSER FUNCTIONS                
-- _____________________________________________________________________________________________________________________________________--
-- "item" parses the first element of a list and return the rest of the list
item :: Parser Char
item = \env inp -> case inp of
                   []     -> []
                   (x:xs) -> [(env,x,xs)] 
                   
-- return a Parser after sostituting the elements of the expression a and Env of the triple with the input values
parserReturn  :: Env -> a -> Parser a
parserReturn newenv v = \env inp -> [(newenv, v,inp)]

-- "failure" stops the parsing
failure :: Parser a
failure  = \env inp -> []

-- CHOICE operator (+++). p +++ q execute p, otherwise if p fails executes q. Input and env are transferred from a parser to the other one
(+++)  :: Parser a -> Parser a -> Parser a
p +++ q = \env inp -> case p env inp of
                   []        -> parse q env inp
                   [(env, v,out)] -> [(env, v,out)]

-- Executes the parsing          
parse :: Parser a -> Env -> String -> [(Env, a,String)]
parse p env inp = p env inp
             
-- SEQUENCING operator (>>>=). parse p >>>= f fails if the application of the parser p to the input string fails, and otherwise applies the function f to the result value to give a
-- second parser, which is then applied to the output string to give the final result
(>>>=) :: Parser a -> (Env -> a -> Parser b ) -> Parser b --This is a MONAD (a box, a function applied to the element in the box, and a box as result in out)
p >>>= f = \env inp -> case parse p env inp of
                       [] -> []   -- If nothing (first parser) in input, parse nothing
                       [(env, v, out)] -> parse (f env v) env out  -- If in input there is the triple, parse the application of the sec parser (function) to the unboxed element v

-- Parse a character if the predicate p is satisfied
sat  :: (Char -> Bool) -> Parser Char
sat p = item >>>= \env x ->
           if p x then
             parserReturn env x
           else
             failure

-- Parse a specific character
char  :: Char -> Parser Char
char x = sat (x ==) 



-- _____________________________________________________________________________________________________________________________________--
--                  ENVIRONMENT MANIPULATIONS                     
-- _____________________________________________________________________________________________________________________________________--
{- "setEnv" sets the environment adding or sobstituting a couple var-val
- v is the NAME of the variable
- a is the VALUE of the var (String type because it will be substituted in the string code)
- es is the environment -}
setEnv :: String -> String -> Env -> Env
setEnv v a []  = [(v,a)]
setEnv v a (e:es)    
              | (fst e)==v      = [(v,a)] ++ es
              | otherwise       = e:(setEnv v a es)

{- "bind" is a function that interpeters all the variable in the env
-- es enviroment of variables
-- xs expression to evaluate -}
bind ::  Env -> String -> String
bind [] xs = xs
bind es [] = []
bind (e:es) xs = bind es (replace e xs) 

{- "replace" replaces a variable in the environment with its value
- v is the couple name,value of the variable
- xs is the expression to evaluate -}
replace :: (String,String) -> String -> String
replace v [] = []
replace v xs
   | (fst v) `isPrefixOf` xs = (snd v) ++ replace v (drop (length (fst v)) xs)
   | otherwise = (xs!!0) : replace v (drop 1 xs)

-- "getCode" extract the code (type of a) from the tuple --
getCode :: [(Env, a, String)] -> a
getCode [(_, x, _)]  =  x

-- extract the env state -- 
getMemory :: [(Env, a, String)] -> String
getMemory []             = []
getMemory [([], _, _)]   = [] 
getMemory [(x:xs, c, s)] = fst x ++ "=>" ++ snd x ++ " " ++ (getMemory [(xs,c,s)])

-- extract the environment from the tuple --
getEnv :: [(Env, a, String)] -> Env
getEnv []            = []
getEnv [([], _, _)]  = [] 
getEnv [(x, _, _)]   = x



-- _____________________________________________________________________________________________________________________________________--
--                      KEYWORDS AND SYMBOLS
-- _____________________________________________________________________________________________________________________________________--

-- Parse the "True" keyword
trueKeyword :: Parser String
trueKeyword  = char 'T' >>>= \env _ -> char 'r' >>>= \_ _ ->  char 'u' >>>= \_ _ -> char 'e' >>>= \_ _ -> parserReturn env ("True")

-- Parse the "False" keyword
falseKeyword :: Parser String
falseKeyword  = char 'F' >>>= \env _ -> char 'a' >>>= \_ _ -> char 'l' >>>= \_ _ -> char 's' >>>= \_ _ -> char 'e' >>>= \_ _ ->  parserReturn env ("False")

-- Parse the ""skip"" keyword
skipKeyword :: Parser String
skipKeyword  = char 's' >>>= \env _ -> char 'k' >>>= \_ _ -> char 'i' >>>= \_ _ -> char 'p' >>>= \_ _ -> parserReturn env ("skip")

-- Parse the "if" keyword
ifKeyword :: Parser String
ifKeyword  = char 'i' >>>= \env _ ->  char 'f' >>>= \_ _ -> space >>>= \_ _ -> parserReturn env "if "
              
-- Parse the "else" keyword
elseKeyword :: Parser String
elseKeyword  =  char 'e' >>>= \env _ -> char 'l' >>>= \_ _ -> char 's' >>>= \_ _ -> char 'e' >>>= \_ _ -> space >>>= \_ _ -> parserReturn env "else "

-- Parse the "while" keyword                        
whileKeyword :: Parser String
whileKeyword  = char 'w' >>>= \env _ -> char 'h' >>>= \_ _ -> char 'i' >>>= \_ _ -> char 'l' >>>= \_ _ -> char 'e' >>>= \_ _ -> space >>>= \_ s -> parserReturn env "while "

-- Parse the "do" keyword                    
doKeyword :: Parser String
doKeyword  = char 'd' >>>= \env _ -> char 'o' >>>= \_ _ -> space >>>= \_ _ -> parserReturn env "do "

-- Parse the "for" keyword                        
forKeyword :: Parser String
forKeyword  = char 'f' >>>= \env _ -> char 'o' >>>= \_ _ -> char 'r' >>>= \_ _ -> space >>>= \_ s -> parserReturn env "for "

-- Parse the "to" keyword                        
toKeyword :: Parser String
toKeyword  = space >>>= \_ s -> char 't' >>>= \env _ -> char 'o' >>>= \_ _ -> space >>>= \_ s -> parserReturn env " to "

-- Parse the "times" keyword
timesKeyword :: Parser String
timesKeyword  = space >>>= \_ s -> char 't' >>>= \env _ -> char 'i' >>>= \_ _ -> char 'm' >>>= \_ _ -> char 'e' >>>= \_ _ -> char 's' >>>= \_ _ -> space >>>= \_ s -> parserReturn env " times "
                        
--      chars parsers
-- Parse the opened graf parentheses
openPargraf :: Parser String
openPargraf   = char '{' >>>= \env _ -> space >>>= \_ _ -> parserReturn env ("{ ")

-- Parse the closed graf parentheses
closePargraf :: Parser String
closePargraf   = char '}' >>>= \env _ ->  parserReturn env ("}")                                           

-- Parse open parentheses (with a possible space prefix)
openPar :: Parser String
openPar = char '(' >>>= \env _ -> parserReturn env "("

-- Parse closed parentheses (with a possible space prefix)
closePar :: Parser String
closePar = char ')' >>>= \env _ -> space >>>= \_ _ -> parserReturn env ") "

-- Parse colon
colon :: Parser String
colon = char ',' >>>= \env _ -> 
             (
              space >>>= \_ _ -> 
                parserReturn env ", "
              ) +++
              parserReturn env ","

-- Parse semicolon
semicolon :: Parser String
semicolon = char ';' >>>= \env _ -> 
              (
              space >>>= \_ _ -> 
              parserReturn env "; "
              ) +++
              parserReturn env ";"

-- Parse a space character
space :: Parser Char
space = char ' '

-- Parse a string of more spaces 
spaces :: Parser String
spaces = char ' ' >>>= \env _ -> 
           parserReturn env " "



-- _____________________________________________________________________________________________________________________________________--
--                      SYNTAX PARSING
-- _____________________________________________________________________________________________________________________________________--

-- ARITHMETIC EXPRESSIONS

-- Digit 0 - 9
digit       :: Parser Char
digit       = sat isDigit

-- Variable
-- variable ::= <letter> | <letter> <variabile>
variable    :: Parser String 
variable    = sat isLetter >>>= \env c ->
                                (
                                  char '[' >>>= \env op ->
                                    variable >>>= \env v ->
                                      char ']' >>>= \env cl ->
                                        parserReturn env ([c] ++ "[" ++ bind env v ++ "]") -- in case array[var] we have to sobstitute the value var (dimension of array) in the env by using  bind funct.
                                )
                                +++
                                (
                                  char '[' >>>= \env op ->
                                    parsenumber >>>= \env num ->
                                      char ']' >>>= \env cl ->
                                        parserReturn env ([c] ++ "[" ++ num ++ "]") -- in case array[n] we just have to parse the array with the dimension (parsenumber)
                                )
                                +++
                                ( 
                                    variable >>>= \env f ->
                                        parserReturn env ([c] ++ f)
                                )
                                +++
                                parserReturn env [c]

-- positive number made of one or more digits
-- positivenumber ::= <digit> | <digit> <positivenumber>
parsepositivenumber :: Parser String
parsepositivenumber = digit >>>= \env d -> (
                                      parsepositivenumber >>>= \env n ->
                                        parserReturn env ([d]++n) 
                                  ) 
                                  +++ parserReturn env [d]
                   
-- Integer number or variable
-- number ::= <positivenumber> | <variable>                        
parsenumber :: Parser String
parsenumber = parsepositivenumber  +++ (variable >>>= \env v -> parserReturn env v) -- substitutes the variables with their values

-- Arithmetic positive factor made of integer numbers or arithmetic expression inside parentheses
-- apositivefactor ::= <number> | ( <aexp> )
parseapositivefactor :: Parser String
parseapositivefactor  = (parsenumber >>>= \env n ->
                          parserReturn env n) 
                        +++ 
                        (char '(' >>>= \env _ ->
                           parseaexpr >>>= \env e ->
                             char ')' >>>= \env _ ->
                               parserReturn env ("(" ++ e ++ ")")
                        )
                        
-- Arithmetic negative factor made by negating positivefactor     
-- anegativefactor ::= - | <apositivefactor>                                     
parseanegativefactor :: Parser String
parseanegativefactor = char '-' >>>= \env _ -> 
                         parseapositivefactor >>>= \env f -> 
                            parserReturn env ("-" ++ f)

-- Arithmetic negative or positive factor          
-- afactor ::= <apositivefactor> | <anegativefactor>            
parseafactor :: Parser String
parseafactor = parseanegativefactor +++ parseapositivefactor 

-- Arithmetic term made of arithmetic factors with moltiplication or division operator
-- aterm ::= <afactor> | <afactor> <aexpOp2> <aterm>
parseaterm :: Parser String
parseaterm  = parseafactor >>>= \env f ->  
                             (
                                 char '*' >>>= \env _ ->
                                   parseaterm >>>= \env t ->
                                     parserReturn env (f ++ "*" ++ t)
                             )
                             +++
                             (
                                 char '/' >>>= \env _ ->
                                   parseaterm >>>= \env t ->
                                     parserReturn env (f ++ "/" ++ t)
                             )
                             +++ parserReturn env f 

-- Arithmetic expression made of arithmetic term with sum or substitution operator
-- aexpr ::= <aterm> | <aterm> <aexpOp1> <aexp>
parseaexpr :: Parser String
parseaexpr  = parseaterm >>>= \env t -> 
                          (
                              char '+' >>>= \env _ ->
                               parseaexpr >>>= \env e ->
                                  parserReturn env (t ++ "+" ++ e)
                          )
                          +++
                          (
                              char '-' >>>= \env _ ->
                                parseaexpr >>>= \env e ->
                                  parserReturn env (t ++ "-" ++ e)
                          )
                          +++ parserReturn env t
                          
-- BOOLEAN EXPRESSIONS

-- Boolean factor made of Arithmetic expression combined by comparison operator
-- bfactor ::= <aexp> | <aexp> <comparisonOp> <aexp> | <variable>
parsebfactor :: Parser String
parsebfactor = (parseaexpr >>>= \env a1 -> 
                          (
                              char '<' >>>= \env _ ->
                                parseaexpr >>>= \env a2 ->
                                  parserReturn env ( a1 ++"<"++ a2)
                          )
                          +++
                          (
                              char '>' >>>= \env _ ->
                                parseaexpr >>>= \env a2 ->
                                  parserReturn env (a1 ++">"++ a2)
                          )
                          +++
                          (
                              char '=' >>>= \env _ ->
                                parseaexpr >>>= \env a3 ->
                                  parserReturn env (a1 ++"="++ a3)
                          ) +++
                          (
                              char '<' >>>= \env _ ->
                                char '=' >>>= \env _ ->
                                parseaexpr >>>= \env a2 ->
                                  parserReturn env ( a1 ++"<="++ a2)
                          )
                          +++
                          (
                              char '>' >>>= \env _ ->
                                char '=' >>>= \env _ ->
                                parseaexpr >>>= \env a2 ->
                                  parserReturn env (a1 ++">="++ a2)
                          )
                          +++
                          (
                              char '!' >>>= \env _ ->
                                char '=' >>>= \env _ ->
                                parseaexpr >>>= \env a3 ->
                                  parserReturn env (a1 ++"!="++ a3)
                          ) 
               ) 
               +++ (variable >>>= \env v ->
                       parserReturn env v)

-- Boolean term made of boolean factors, a boolean expression inside parentheses and negation operator
-- bterm ::= <bfactor> | ( <bexp> ) | ! <bexp>
parsebterm :: Parser String
parsebterm = 
             ((trueKeyword +++ falseKeyword) >>>= \env b1 -> parserReturn env b1) +++
             (parsebfactor >>>= \env b2 -> parserReturn env b2) +++
             ((trueKeyword +++ falseKeyword) >>>= \env b2 -> parserReturn env b2) +++
             (char '(' >>>= \env _ -> parsebexpr >>>= \env b3 -> char ')' >>>= \env _ -> parserReturn env ( "(" ++ b3 ++ ")") ) +++
             (char '!' >>>= \env _ -> parsebexpr >>>= \env b4 -> parserReturn env ("!"++ b4))

-- Boolean expression made of boolean terms with And and Or operator
-- bexp ::= <bterm> | <bterm> <bexpOp> <bexp>
parsebexpr :: Parser String
parsebexpr = parsebterm >>>= \env b1 ->  
                            (
                                char '&' >>>= \env _ ->
                                  parsebexpr >>>= \env b2 ->
                                    parserReturn env ( b1 ++ "&" ++ b2)
                            )                         
                            +++
                            (
                                char '|' >>>= \env _ ->
                                  parsebexpr >>>= \env b2 ->
                                    parserReturn env (b1 ++ "|" ++ b2)
                            )
                            +++ parserReturn env b1

-- COMMAND EXPRESSIONS
-- assignment Command
-- assignmentcommand ::= <variable> := (<aexp> | <bexp>) <semicolon> | <letter> := { <array> } <semicolon> 
parseassignmentCommand :: Parser String
parseassignmentCommand = variable >>>= \env v ->
                          char ':' >>>= \env _ -> 
                            char '=' >>>= \env _ -> 
                              (
                                parsebexpr >>>= \env b ->
                                  semicolon >>>= \env s ->   
                                    parserReturn env (v ++ ":=" ++ b ++ s)
                              ) 
                              +++
                              (   
                                parseaexpr >>>= \env a ->
                                  semicolon >>>= \env s ->  
                                    parserReturn env (v ++ ":=" ++ a ++ s)
                              )
                              +++
                              ( --parse the array explicit declaration
                                char '{' >>>= \env opg ->  
                                  parsearray >>>= \env arr -> --this function is defined after the parser of the assignment command
                                    char '}' >>>= \env cpg ->
                                      semicolon >>>= \env s ->
                                        parserReturn env (v ++ ":=" ++ [opg] ++ arr ++ [cpg] ++ s)
                              )

-- Parses the array elements when an array is explicit declared as a sequence of factors
-- array ::= <afactor> | <afactor> <colon> <array>
parsearray :: Parser String                
parsearray =  parseafactor >>>= \env n -> 
               (  
                  colon >>>= \env c ->    
                  parsearray >>>= \env f ->  
                  parserReturn env (n ++ c ++ f)
               )
               +++
               parserReturn env (n)  

-- if Command
  -- ifcommand ::= if <space> ( <bexp> ) <space> then { <space> (<program> | <program> else { <space> <program>) } <semicolon>
parseifCommand :: Parser String
parseifCommand = ifKeyword >>>= \env i ->
                   openPar >>>= \env op ->
                     parsebexpr >>>= \env b -> 
                       closePar >>>= \env cp ->
                          openPargraf >>>= \env t ->
                            parseprogram >>>= \env p1 -> 
                              (                          
                                 elseKeyword >>>= \env e ->
                                   parseprogram >>>= \env p2 ->
                                     closePargraf >>>= \env ei ->
                                      semicolon >>>= \env se -> 
                                       parserReturn env (i ++ op ++ b ++ cp ++ t  ++ p1 ++ e ++ p2 ++ ei ++ se)
                           ) +++
                           ( 
                              closePargraf >>>= \env ei ->
                              semicolon >>>= \env se ->  
                              parserReturn env (i ++ op ++ b ++ cp ++ t  ++ p1 ++ ei ++ se)
                           )

-- While command
-- whilecommand ::= while <space> ( <bexp> ) <space> { <space> do <space> <program> <space> } <semicolon>
parsewhileCommand :: Parser String
parsewhileCommand = whileKeyword >>>= \env w ->
                      openPar >>>= \env op ->
                        parsebexpr >>>= \env b -> 
                          closePar >>>= \env cp -> 
                            openPargraf >>>= \env gr -> 
                              doKeyword >>>= \env t1 -> 
                                parseprogram >>>= \env p ->
                                  closePargraf >>>= \env ew ->
                                  semicolon >>>= \env s -> 
                                   parserReturn env (w ++ op ++ b ++ cp ++ gr ++ t1 ++ p ++ ew ++ s)

-- Do While command
-- dowhilecommand ::= do <space> { <space> do <space> <program> <space> } while <space> ( <bexp> ) <space> <semicolon>
parsedowhileCommand  :: Parser String
parsedowhileCommand  = doKeyword >>>= \env d ->
                          openPargraf >>>= \env opg ->
                            parseprogram >>>= \env p ->
                              closePargraf >>>= \env cpg ->
                                whileKeyword >>>= \env w ->
                                  openPar >>>= \env op ->
                                    parsebexpr >>>= \env b ->
                                      closePar >>>= \env cp ->
                                        semicolon >>>= \env s ->
                                          parserReturn env (d ++ opg ++ p ++ cpg ++ w ++ op ++ b ++ cp ++ s)

-- For Times command
-- forcommand ::= for <space> <variable> <space> times <space> { <space> <program> <space> }
parseforCommand :: Parser String
parseforCommand = forKeyword >>>= \env f ->
                    variable >>>= \env v ->
                      timesKeyword >>>=  \env t ->
                          openPargraf >>>= \env op ->
                            parseprogram >>>= \env p ->
                              closePargraf >>>= \env cp ->
                                semicolon >>>= \env s ->
                                  parserReturn env (f ++ v ++ t ++ op ++ p ++ cp ++ s) 


-- Command can be skip, assignment, if, while, do while or for times 
-- command ::= <skipcommand> | <assignmentcommand> | <ifcommand> | <whilecommand> | <dowhilecommand> | <forcommand>
parsecommand :: Parser String
parsecommand = (skipCommand +++ parseassignmentCommand  +++ parseifCommand +++ parsewhileCommand +++ parsedowhileCommand +++ parseforCommand) >>>= \env c -> 
                   parserReturn env c

-- program ::= <command> | <command> <program>
parseprogram :: Parser String
parseprogram = parsecommand >>>= \env c -> ( parseprogram >>>= \env p -> parserReturn env (c ++ p)) +++ parserReturn env c



-- _____________________________________________________________________________________________________________________________________--
--                      EXPRESSION'S EVALUATION
-- _____________________________________________________________________________________________________________________________________--

-- ARITHMETIC EXPRESSIONS

-- Positive number are made of one or more digits (SAME of the syntax parsing -> we have just to collect the string of the pos. number)
-- positivenumber ::= <digit> | <digit> <positivenumber>
positivenumber      :: Parser String
positivenumber      = digit >>>= \env d ->
                                (
                                  positivenumber >>>= \env n ->
                                    parserReturn env ([d] ++ n)
                                )
                                +++
                                parserReturn env [d]

-- Integer number or variable    
-- number ::= <positivenumber> | <variable>                    
number              :: Parser String
number              = positivenumber  +++ 
                                    (
                                      variable >>>= \env v -> 
                                        parserReturn env (bind env v) -- in case of var we have to bind wrt the only syuntax check 
                                    ) -- substitutes the variables with their values

-- Arithmetic positive factor made of integer numbers or arithmetic expression inside parentheses
-- apositivefactor ::= <number> | ( <aexp> )
apositivefactor     :: Parser Int 
apositivefactor     = (
                        number >>>= \env n -> 
                          parserReturn env (read n :: Int ) -- Different from the only syntax parsing: we have to cast the string and memorize in the env the value (int)
                      )
                      +++
                      (
                        char '(' >>>= \env _ ->
                          aexpr >>>= \env e ->
                            char ')' >>>= \env _ ->
                              parserReturn env e
                      )

-- Arithmetic negative factor made by negating positivefactor      
-- anegativefactor ::= - | <apositivefactor>                                    
anegativefactor     :: Parser Int 
anegativefactor     = char '-' >>>= \env _ ->
                        apositivefactor >>>= \env f ->
                          parserReturn env (f * (-1))       -- Different, but just to reverse sign

-- Arithmetic can be negative or positive factor   
-- afactor ::= <apositivefactor> | <anegativefactor>                   
afactor            :: Parser Int
afactor            = anegativefactor +++ apositivefactor 

-- Evaluation of the aexpr wich contains * or /
-- aterm ::= <afactor> | <afactor> <aexpOp2> <aterm>
aterm              :: Parser Int 
aterm              = afactor >>>= \env f ->
                                (
                                  char '*' >>>= \env _ ->
                                    aterm >>>= \env t ->
                                      parserReturn env (f * t)  -- here operands can be applied because the values have been sobstituted by the casting in the previous defined functions
                                )
                                +++
                                (
                                  char '/' >>>= \env _ ->
                                    aterm >>>= \env t ->
                                      parserReturn env (fromIntegral (div f t))  -- fromintegral converts from integer to int
                                )
                                +++
                                parserReturn env f

-- Evaluation of arithmetic expr
-- aexpr ::= <aterm> | <aterm> <aexpOp1> <aexp>
aexpr               :: Parser Int 
aexpr               = aterm >>>= \env t ->
                                (
                                  char '+' >>>= \env _ ->
                                    aexpr >>>= \env e -> 
                                      parserReturn env (t + e)
                                )
                                +++
                                (
                                  char '-' >>>= \env _ ->
                                    aexpr >>>= \env e -> 
                                      parserReturn env (t - e)
                                )
                                +++
                                parserReturn env t

-- BOOLEAN EXPRESSIONS

-- evaluation of Boolean factor made of Arithmetic expression combined by comparison operator         
-- bfactor ::= <aexp> | <aexp> <comparisonOp> <aexp> | <variable>        
bfactor             :: Parser Bool
bfactor             = (aexpr >>>= \env a1 ->
                                (
                                  char '<' >>>= \env _ ->
                                    aexpr >>>= \env a2 ->
                                      parserReturn env (a1 < a2)
                                )
                                +++ 
                                (
                                  char '>' >>>= \env _ ->
                                    aexpr >>>= \env a2 ->
                                      parserReturn env (a1 > a2)
                                )
                                +++ 
                                (
                                  char '=' >>>= \env _ ->
                                    aexpr >>>= \env a3 ->
                                      parserReturn env (a1 == a3)
                                )
                                +++
                                (
                                  char '<' >>>= \env _ ->
                                    char '=' >>>= \env _ ->
                                      aexpr >>>= \env a2 ->
                                        parserReturn env (a1 <= a2)
                                )
                                +++
                                (
                                  char '>' >>>= \env _ ->
                                    char '=' >>>= \env _ ->
                                      aexpr >>>= \env a2 ->
                                        parserReturn env (a1 >= a2)
                                )
                                +++
                                (
                                  char '!' >>>= \env _ ->
                                    char '=' >>>= \env _ ->
                                      aexpr >>>= \env a2 ->
                                        parserReturn env (a1 >= a2)
                                )
                      )
                      +++
                      (
                        variable >>>= \env v ->
                          if ((bind env v)  == "True") || ((bind env v)  == "False")
                            then 
                              parserReturn env (read (bind env v) :: Bool) -- To avoid a type missmatch substitutes the variables with their values and cast string in boolean 
                            else
                              failure
                      )
-- Evaluation of bterm
-- bterm ::= <bfactor> | ( <bexp> ) | ! <bexp>
bterm :: Parser Bool
bterm = 
        ((trueKeyword +++ falseKeyword) >>>= \env b1 -> parserReturn env (read b1 :: Bool)) +++
        (bfactor >>>= \env b2 -> parserReturn env b2) +++      
        (char '(' >>>= \env _ -> bexpr >>>= \env b3 -> char ')' >>>= \env _ -> parserReturn env b3) +++
        (char '!' >>>= \env _ -> bexpr >>>= \env b4 -> parserReturn env (not b4))

-- Evaluation of bexpr
-- bexp ::= <bterm> | <bterm> <bexpOp> <bexp>
bexpr             :: Parser Bool
bexpr             = bterm >>>= \env b1 ->
                              (
                                char '&' >>>= \env _ ->
                                  bexpr >>>= \env b2 ->
                                    parserReturn env (b1 && b2)
                              )
                              +++
                              (
                                char '|' >>>= \env _ ->
                                  bexpr >>>= \env b2 ->
                                    parserReturn env (b1 || b2)
                              )
                              +++
                              parserReturn env b1

--      COMMAND EXPRESSIONS

-- Skip command is composed of skip keyword and semicolon
-- skipcommand ::= skip <semicolon>
skipCommand       :: Parser String
skipCommand       = skipKeyword >>>= \env sk ->
                                    semicolon  >>>= \_ s ->
                                      parserReturn env (s ++ sk)

-- This function recognizes the assignment command, in particular the variable
-- and the assigned value (integer or Boolean), and also the assigment to an element of an array.
-- assignmentcommand ::= <variable> := (<aexp> | <bexp>) <semicolon> | <letter> := { <array> } <semicolon>  
assignmentCommand :: Parser String
assignmentCommand = (variable >>>= \env v ->
                        char ':' >>>= \env _ -> 
                          char '=' >>>= \env _ -> 
                            (
                              bexpr >>>= \env b ->  
                                semicolon >>>= \env s -> 
                                  parserReturn (setEnv v (show b) env) (v ++ ":=" ++ (show b) ++ s)
                            ) 
                            +++
                            (  
                               aexpr >>>= \env a -> 
                                 semicolon >>>= \env s -> 
                                  parserReturn (setEnv v (show a) env) (v ++ ":=" ++ (show a) ++ s)
                            ))
                            +++
                            (sat isLetter >>>= \env v ->
                             char ':' >>>= \env _ -> 
                             char '=' >>>= \env _ -> 
                             char '{' >>>= \env op ->
                             arrayType  >>>= \env ar -> --this read the array elements, so it parse a list of integer
                             char '}' >>>= \env cp ->  
                             semicolon >>>= \env s ->  
                             parserReturn (saveArray env [v] ar ) ([v] ++ ":=" ++ [op] ++ (show ar) ++ [cp] ++ s) -- in order to save the array, manipulate env with savearray
                            )

-- The functions for saving arrays are arrayType and saveArray. The first, arrayType returns an [int] list 
-- from the parsing of the elements, taken as a parameter by savearray with the env and the variable, 
-- to save the single elements in the env, i.e. y:={2,5}; -> y[0]:=2; y[1]:=5;
-- array ::= <afactor> | <afactor> <colon> <array>
arrayType ::  Parser [Int]
arrayType  = afactor  >>>= \env n -> 
                (
                   colon >>>= \env c ->
                     arrayType >>>= \env f ->
                       parserReturn env ([n] ++ f)
                  )
                   +++ 
                   parserReturn env [n]

saveArray :: Env -> String -> [Int] -> Env
                      --foldl :: (a -> b -> a) -> a -> [b] -> a
saveArray env var list = foldl (\e v -> setEnv (fst v) (snd v) e) env l  -- in the v = [(name, value)]
                        -- zipWith:: 	(a -> b -> c) -> [a] -> [b] -> [c] makes a list, its elements are calculated from the function and the elements of input lists occuring at the same position in both lists
                         where l = zipWith (\val index ->     -- so this we are going to memorize in the env in form of string
                                  (var ++ "[" ++ (show index) ++ "]", show (val) )) list [0..] -- So the list in out is a string of this pattern
                              
-- For the For statement, LIMPid first evaluates the value of the variable that represents the number of iterations of the program that will be execute, 
-- then parses the program (parseprogram) without its  evaluation and computes the function repeatNTimes
-- forcommand ::= for <space> <variable> <space> times <space> { <space> <program> <space> }
forCommand :: Parser String
forCommand = forKeyword >>>= \env f ->  
              variable >>>= \env v ->
                timesKeyword >>>= \env t ->
                    openPargraf >>>= \env op ->
                      parseprogram >>>= \env p ->
                        repeatNTimes env p v >>>= \env r ->
                          closePargraf >>>= \env cp ->
                            semicolon >>>= \env s ->
                            parserReturn env (f ++ v ++ t ++ op ++ r ++ cp ++ s)

-- RepeatNTimes takes as parameters the env with the variable for the iteration evaluated 
-- and the program parsed but not executed.
-- So, if there are not iteration to be executed (the variable is zero or a negative number), 
-- then LIMPid leaves the program only parsed and not evaluated.
repeatNTimes :: Env -> String -> String  -> Parser String
repeatNTimes env p v = if bind env v <= "0" then -- Condition is evaluated at each iteration of the for statement
                              parserReturn env p
                          else 
                            parserReturn env (v ++ ":=" ++ v ++ "-" ++ "1" ++ ";") >>>= \env dec->     -- Is like we read in the code this statement
                                parserReturn  (getEnv (parse program env dec)) dec  >>>= \envdec _ ->  -- And we use it to update the env with such a statement
                                  parserReturn (getEnv (parse program envdec p)) p >>>= \envf _ -> 
                                    repeatNTimes envf p v -- executes again the for with the new environment in a recursively way
                    

-- If command
-- ifcommand ::= if <space> ( <bexp> ) <space> then { <space> (<program> | <program> else { <space> <program>) } <semicolon>
ifCommand         :: Parser String
ifCommand         = ifKeyword >>>= \env i ->
                      openPar >>>= \env op ->
                        bexpr >>>= \env b ->
                          closePar >>>= \env cp ->
                            openPargraf >>>= \env t ->
                              if b 
                                then
                                  program >>>= \envTrue p1 ->
                                    (
                                      elseKeyword >>>= \env e ->
                                        parseprogram >>>= \env p2 ->
                                          closePargraf >>>= \env ei ->
                                            semicolon >>>= \env s ->
                                              parserReturn envTrue (i ++ op ++ (show b) ++ cp ++ t ++ p1 ++ e ++ p2 ++ ei ++ s)
                                    )
                                    +++ --whithout else branch
                                    (
                                      closePargraf >>>= \env ei ->
                                        semicolon >>>= \env s ->
                                          parserReturn envTrue (i ++ op ++ (show b) ++ cp ++ t ++ p1 ++ ei ++ s)
                                    )
                                else
                                  parseprogram >>>= \env p1 ->
                                    (
                                      elseKeyword >>>= \env e ->
                                        program >>>= \envFalse p2 ->
                                          closePargraf >>>= \env ei ->
                                            semicolon >>>= \env s ->
                                              parserReturn envFalse (i ++ op ++ (show b) ++ cp ++ t  ++ p1 ++ e ++ p2 ++ ei ++ s)
                                    )
                                    +++
                                    (
                                      closePargraf >>>= \env ei ->
                                        semicolon >>>= \env s ->
                                          parserReturn env (i ++ op ++ (show b) ++ cp ++ t ++ p1 ++ ei ++ s)
                                    )

-- While command
-- whilecommand ::= while <space> ( <bexp> ) <space> { <space> do <space> <program> <space> } <semicolon>
whileCommand        :: Parser String
whileCommand        = whileKeyword >>>= \env w ->
                        openPar >>>= \env op ->
                          parsebexpr >>>= \env b ->
                            closePar >>>= \env cp ->
                              openPargraf >>>= \env ogr ->
                                doKeyword >>>= \env t1 ->
                                  parseprogram >>>= \env p ->
                                    closePargraf >>>= \env cgr ->
                                      semicolon >>>= \env s ->
                                        if (getCode (parse bexpr env b)) -- the bexpr is re-evaluated every cicle 
                                          then 
                                            parserReturn (getEnv (parse program env p)) p >>>= \envw _ -> -- execution of the p program inside the while
                                              parserReturn (getEnv (parse program envw (w ++ op ++ b ++ cp ++ ogr ++ t1 ++ p ++ cgr ++ s))) (w ++ op ++ b ++ cp ++ ogr ++ t1 ++ p ++ cgr ++ s) -- re-execution of the same commands
                                          else
                                            parserReturn env (w ++ op ++ b ++ cp ++ ogr ++ t1 ++ p ++ cgr ++ s)

-- Do While command parserReturn env (d ++ opg ++ p ++ cpg ++ op ++ b ++ cp ++ s)
-- dowhilecommand ::= do <space> { <space> do <space> <program> <space> } while <space> ( <bexp> ) <space> <semicolon>
dowhileCommand  :: Parser String
dowhileCommand  = doKeyword >>>= \env d ->
                          openPargraf >>>= \env opg ->
                            parseprogram >>>= \env p ->
                              closePargraf >>>= \env cpg ->
                                whileKeyword >>>= \env w ->
                                  openPar >>>= \env op ->
                                    parsebexpr >>>= \env b ->
                                      closePar >>>= \env cp ->
                                        semicolon >>>= \env s ->
                                          parserReturn (getEnv (parse program env p)) p >>>= \envw _ -> -- execution of the p program inside the do - block for the first iteration
                                            if (getCode (parse bexpr env b)) -- the bexpr is re-evaluated every cicle 
                                              then -- program reexcuted with the new env and the execution of the code
                                                parserReturn (getEnv (parse program envw (d ++ opg ++ p ++ cpg ++ w ++ op ++ b ++ cp ++ s))) (d ++ opg ++ p ++ cpg ++ w ++ op ++ b ++ cp ++ s)
                                              else
                                                parserReturn env (d ++ opg ++ p ++ cpg ++ w ++ op ++ b ++ cp ++ s)
 
-- Command can be skip, assignment, if, while, for, arithmetic expression or boolean expression
command :: Parser String
command = (skipCommand +++ assignmentCommand  +++ ifCommand +++ whileCommand +++ forCommand +++ dowhileCommand) >>>= \env c -> 
                   parserReturn env c

-- Program is a set of command or a single command
program :: Parser String
program = command >>>= \env c -> ( program >>>= \env p -> parserReturn env (c ++ p)) +++ parserReturn env c



-- +++++++++++++++++++++++++++  INTERACTIVE SHELL +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

parser  :: String -> IO String
parser xs =
    do
      putStr "LIMPid#>"
      hFlush stdout -- flushes all the buffered output data
      ys <- getLine 
      case ys of
        ":printmem" ->
          do 
            -- here code parsing and memory situation
            putStrLn ""
            putStrLn "-+-+ Parsed Code +-+-"
            if xs == [] then
              putStrLn ""
            else
              putStrLn (getCode (parse parseprogram [] xs)) -- here code parsing
            putStrLn ""
            putStrLn "-+-+ Memory +-+-"
            putStrLn (getMemory (parse program [] xs))      -- like a tostring to see the mem situation, here the real execution of the program
            putStrLn ""
            parser xs
        
        ":syntax" ->
          do 
            -- here output the formal grammar
            putStrLn "-+-+ LIMPid Syntax +-+-"
            putStrLn ""
            putStrLn  "  program := <command> | <command> <program>"
            putStrLn  "  command ::= <skipcommand> | <assignmentcommand> | <ifcommand> | <whilecommand>"
            putStrLn  "  skipcommand ::= skip <semicolon>"
            putStrLn  "  assignmentcommand ::= <variable> := (<aexp> | <bexp>) <semicolon> | <letter> := { <array> } <semicolon>   "
            putStrLn  "  array ::= <afactor> | <afactor> <colon> <array> "
            putStrLn  "  ifcommand ::= if <space> ( <bexp> ) <space> { <space> (<program> | <program> else <space> <program>) } <semicolon>"
            putStrLn  "  whilecommand ::= while <space> ( <bexp> ) <space> { <space> do <space> <program>  <space> } <semicolon>"
            putStrLn  "  dowhilecommand ::= do <space> { <space> do <space> <program> <space> } while <space> ( <bexp> ) <space> <semicolon>"
            putStrLn  "  forcommand ::= for <space> <variable> <space> times <space> { <space> <program> <space> } "
            putStrLn  "  bexp ::= <bterm> | <bterm> <bexpOp> <bexp>"
            putStrLn  "  bterm ::= <bfactor> | ( <bexp> ) | ! <bexp>"
            putStrLn  "  bfactor ::= <aexp> | <aexp> <comparisonOp> <aexp> | <variable>"
            putStrLn  "  aexpr ::= <aterm> | <aterm> <aexpOp1> <aexp>"
            putStrLn  "  aterm ::= <afactor> | <afactor> <aexpOp2> <aterm>"
            putStrLn  "  afactor ::= <apositivefactor> | <anegativefactor>"
            putStrLn  "  anegativefactor ::= - | <apositivefactor>"
            putStrLn  "  apositivefactor ::= <number> | ( <aexp> )"
            putStrLn  "  number ::= <positivenumber> | <variable>"
            putStrLn  "  positivenumber ::= <digit> | <digit> <positivenumber>"
            putStrLn  "  variable ::= <letter> | <letter> <variabile>"
            putStrLn  "  semicolon ::= ; | ; <space>"
            putStrLn  "  digit ::= 0-9"
            putStrLn  "  aexpOp1 ::= + | -"
            putStrLn  "  aexpOp2 ::=  * | /"
            putStrLn  "  bexpOp ::= & | |"
            putStrLn  "  comparisonOp ::= < | > | = | <= | >= | !="
            putStrLn  "  letter ::= a-z"
            putStrLn  "  space ::= \" \" "
            parser (xs)
        
        ":help" ->
          do
            -- here the Help section whith the explanations of the possible commands
            putStrLn "-+-+ LIMPid Help +-+-"
            putStrLn  ""
            putStrLn  "  :printmem      prints the parsed code and the status of the memory"
            putStrLn  ""
            putStrLn  "  :syntax        prints the LIMPid formal grammar"
            putStrLn  ""
            putStrLn  "  :help          prints the help with the commands of the program"
            putStrLn  ""
            putStrLn  "  :quit          stops the LIMPid program"
            putStrLn  ""
            parser xs
        
        ":quit" ->
          do
            -- quit from the interpeter's shell
            return []
        
        otherwise ->
          -- Error situation (input in the shell is not a valid command) JUST SYNTAX CHECK
          case parse parseprogram [] ys of
            [] ->
              do
                putStrLn "Syntax error! Please type \":help\""
                parser xs
            otherwise -> parser (xs ++ ys)



-- Call this functions to start the program (the Interpreter)
limpid :: IO String
limpid = do
            putStrLn ""
            putStrLn ""
            putStrLn ""
            putStrLn ""
            putStrLn "-+-+-+-+-+- LIMPid Language Interpreter -+-+-+-+-+-"
            putStrLn ""
            putStrLn "Type \":help\" for commands"
            putStrLn ""
            putStrLn ""
            parser []