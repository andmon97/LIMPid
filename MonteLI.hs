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

-- "parserReturn" modify the environment after an assignment
-- return a Parser after sostituting the elements of the expression a and Env of the triple with the input values
parserReturn          :: Env -> a -> Parser a
parserReturn newenv v = \env inp -> [(newenv, v, inp)]

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
                        parserReturn env x
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
trueKeyword     = char 'T' >>>= \env _ -> char 'r' >>>= \_ _ -> char 'u' >>>= \_ _ -> char 'e' >>>= \_ _ -> parserReturn env ("True")

-- Parse the "False" keyword
falseKeyword    :: Parser String
falseKeyword    = char 'F' >>>= \env _ -> char 'a' >>>= \_ _ -> char 'l' >>>= \_ _ -> char 's' >>>= \_ _ -> char 'e' >>>= \_ _ -> parserReturn env ("False")

-- Parse the "skip" keyword
skipKeyword     :: Parser String
skipKeyword     = char 's' >>>= \env _ -> char 'k' >>>= \_ _ -> char 'i' >>>= \_ _ -> char 'p' >>>= \_ _ -> parserReturn env ("skip")

-- Parse the "if" keyword
ifKeyword       :: Parser String
ifKeyword       = char 'i' >>>= \env _ -> char 'f' >>>= \_ _ -> space >>>= \_ _ -> parserReturn env ("if ")

-- Parse the "else" keyword
elseKeyword     :: Parser String
elseKeyword     = char 'e' >>>= \env _ -> char 'l' >>>= \_ _ -> char 's' >>>= \_ _ -> char 'e' >>>= \_ _ -> space >>>= \_ _ -> parserReturn env ("else ")

-- Parse the "do" keyword
doKeyword       :: Parser String
doKeyword       = char 'd' >>>= \env _ -> char 'o' >>>= \_ _ -> space >>>= \_ _ -> parserReturn env ("do ")

-- Parse the "while" keyword
whileKeyword    :: Parser String
whileKeyword    = char 'w' >>>= \env _ -> char 'h' >>>= \_ _ -> char 'i' >>>= \_ _ -> char 'l'  >>>= \_ _ -> char 'e' >>>= \_ _ -> space >>>= \_ _ -> parserReturn env ("while ")

--      chars parsers
-- Parse the opened pargraf
openPargraf     :: Parser String 
openPargraf     = char '{' >>>= \env _ -> space >>>= \_ _ -> parserReturn env ("{ ")

-- Parse the closed pargraf
closePargraf   :: Parser String
closePargraf   = char '}' >>>= \env _ -> parserReturn env ("}")

-- Parse the opened square parenth.
openPar        :: Parser String 
openPar        = char '(' >>>= \env _ -> parserReturn env (")")

-- Parse the closed square parenth.
closePar        :: Parser String 
closePar        = char ')' >>>= \env _ -> space >>>= \_ _ -> parserReturn env (")")

-- Parse the colon
colon           :: Parser String
colon           = char ',' >>>= \env _ ->
                                        (
                                            space >>>= \_ _ ->
                                            parserReturn env ", "
                                        )
                                        +++
                                        parserReturn env ","

-- Parse the semicolon
semicolon       :: Parser String
semicolon       = char ',' >>>= \env _ ->
                                        (
                                            space >>>= \_ _ ->
                                            parserReturn env "; "
                                        )
                                        +++
                                        parserReturn env ";"

-- Parse the space character
space           :: Parser Char
space           = char ' '

-- Parse a string of spaces
spaces          :: Parser String 
spaces          = char ' ' >>>= \env _ -> parserReturn env " "



-- _____________________________________________________________________________________________________________________________________--
--                      SYNTAX PARSING
-- _____________________________________________________________________________________________________________________________________--

-- ARITHMETIC EXPRESSIONS

-- Digit 0 - 9
digit       :: Parser Char
digit       = sat isDigit

-- Variable
variable    :: Parser String 
variable    = sat isLetter >>>= \env c ->
                                ( 
                                    variable >>>= \env f ->
                                        parserReturn env ([c] ++ f)
                                )
                                +++
                                parserReturn env [c]

-- positive number made of one or more digits
parsepositivenumber :: Parser String
parsepositivenumber = digit >>>= \env d -> (
                                      parsepositivenumber >>>= \env n ->
                                        parserReturn env ([d]++n) 
                                  ) 
                                  +++ parserReturn env [d]
                   
-- Integer number or variable                        
parsenumber :: Parser String
parsenumber = parsepositivenumber  +++ (variable >>>= \env v -> parserReturn env v) -- substitutes the variables with their values

-- Arithmetic positive factor made of integer numbers or arithmetic expression inside parentheses
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
parseanegativefactor :: Parser String
parseanegativefactor = char '-' >>>= \env _ -> 
                         parseapositivefactor >>>= \env f -> 
                            parserReturn env ("-" ++ f)

-- Arithmetic negative or positive factor                      
parseafactor :: Parser String
parseafactor = parseanegativefactor +++ parseapositivefactor 

-- Arithmetic term made of arithmetic factors with moltiplication or division operator
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
parsebterm :: Parser String
parsebterm = 
             ((trueKeyword +++ falseKeyword) >>>= \env b1 -> parserReturn env b1) +++
             (parsebfactor >>>= \env b2 -> parserReturn env b2) +++
             ((trueKeyword +++ falseKeyword) >>>= \env b2 -> parserReturn env b2) +++
             (char '(' >>>= \env _ -> parsebexpr >>>= \env b3 -> char ')' >>>= \env _ -> parserReturn env ( "(" ++ b3 ++ ")") ) +++
             (char '!' >>>= \env _ -> parsebexpr >>>= \env b4 -> parserReturn env ("!"++ b4))

-- Boolean expression made of boolean terms with And and Or operator
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
parseassignmentCommand :: Parser String
parseassignmentCommand = variable >>>= \env v ->
                        char ':' >>>= \env _ -> 
                          char '=' >>>= \env _ -> 
                            (
                              parsebexpr >>>= \env b ->
                                semicolon >>>= \env s ->   
                                  parserReturn env (v ++ ":=" ++ b ++ s)
                            ) +++
                            (   
                               parseaexpr >>>= \env a ->
                                 semicolon >>>= \env s ->  
                                  parserReturn env (v ++ ":=" ++ a ++ s)
                            )

-- if Command
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
                                 

-- Command can be skip, assignment, if or while
parsecommand :: Parser String
parsecommand = (skipCommand +++ parseassignmentCommand  +++ parseifCommand +++ parsewhileCommand) >>>= \env c -> 
                   parserReturn env c


parseprogram :: Parser String
parseprogram = parsecommand >>>= \env c -> ( parseprogram >>>= \env p -> parserReturn env (c ++ p)) +++ parserReturn env c



-- _____________________________________________________________________________________________________________________________________--
--                      EXPRESSION'S EVALUATION
-- _____________________________________________________________________________________________________________________________________--

-- ARITHMETIC EXPRESSIONS

-- Positive number are made of one or more digits
positivenumber      :: Parser String
positivenumber      = digit >>>= \env d ->
                                (
                                  positivenumber >>>= \env n ->
                                    parserReturn env ([d] ++ n)
                                )
                                +++
                                parserReturn env [d]

-- Integer number or variable                        
number              :: Parser String
number              = positivenumber  +++ 
                                    (
                                      variable >>>= \env v -> 
                                        parserReturn env (bind env v)
                                    ) -- substitutes the variables with their values

-- Arithmetic positive factor made of integer numbers or arithmetic expression inside parentheses
apositivefactor     :: Parser Int 
apositivefactor     = (
                        number >>>= \env n -> 
                          parserReturn env (read n :: Int )
                      )
                      +++
                      (
                        char '(' >>>= \env _ ->
                          aexpr >>>= \env e ->
                            char ')' >>>= \env _ ->
                              parserReturn env e
                      )

-- Arithmetic negative factor made by negating positivefactor                                          
anegativefactor     :: Parser Int 
anegativefactor     = char '-' >>>= \env _ ->
                        apositivefactor >>>= \env f ->
                          parserReturn env (f * (-1))

-- Arithmetic can be negative or positive factor                      
afactor            :: Parser Int
afactor            = anegativefactor +++ apositivefactor 

-- Evaluation of the aexpr wich contains * or /
aterm              :: Parser Int 
aterm              = afactor >>>= \env f ->
                                (
                                  char '*' >>>= \env _ ->
                                    aterm >>>= \env t ->
                                      parserReturn env (f * t)  -- here operands can be applied because the values have been sobstituted by the bind
                                )
                                +++
                                (
                                  char '/' >>>= \env _ ->
                                    aterm >>>= \env t ->
                                      parserReturn env (fromIntegral (div f t))  -- fromintegral converts from integer to int
                                )
                                +++
                                parserReturn env f

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

-- Boolean factor made of Arithmetic expression combined by comparison operator                 
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

bterm :: Parser Bool
bterm = 
        ((trueKeyword +++ falseKeyword) >>>= \env b1 -> parserReturn env (read b1 :: Bool)) +++
        (bfactor >>>= \env b2 -> parserReturn env b2) +++      
        (char '(' >>>= \env _ -> bexpr >>>= \env b3 -> char ')' >>>= \env _ -> parserReturn env b3) +++
        (char '!' >>>= \env _ -> bexpr >>>= \env b4 -> parserReturn env (not b4))

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
skipCommand       :: Parser String
skipCommand       = skipKeyword >>>= \env sk ->
                                    semicolon  >>>= \_ s ->
                                      parserReturn env (s ++ sk)

-- Assignment command
assignmentCommand :: Parser String
assignmentCommand = (variable >>>= \env v->
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

-- If command
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

-- Command can be skip, assignment, if, while, arithmetic expression or boolean expression
command             :: Parser String
command             = (skipCommand +++ assignmentCommand  +++ ifCommand +++ whileCommand) >>>= \env c -> 
                        parserReturn env c

-- Program is a set of command or a single command
program             :: Parser String
program             = command >>>= \env c -> 
                        (
                          program >>>= \env p -> parserReturn env (c ++ p)
                        ) 
                        +++ 
                        parserReturn env c



-- +++++++++++++++++++++++++++  INTERACTIVE SHELL +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

parser  :: String -> IO String
parser xs =
    do
      putStr "MonteLI#>"
      hFlush stdout -- flushes all the buffered output data
      ys <- getLine 
      case ys of
        ":mem" ->
          do 
            -- here code parsing and memory situation
            parser xs
        
        ":MonteLIsyntax" ->
          do 
            -- here output the formal grammar
            parser xs
        
        ":help" ->
          do
            -- here the Help section whith the explanations of the possible commands
            parser xs
        
        ":quit" ->
          do
            -- quit from the interpeter's shell
            return []
        
        otherwise ->
          -- Error situation (input in the shell is not a valid command)
          case parse parseprogram [] ys of
            [] ->
              do
                putStrLn "Syntax error! Please type \":help\""
                parser xs
            otherwise -> parser (xs ++ ys)
            