import Text.ParserCombinators.Parsec

data Type = TypeInt 
        | TypeVar Name
        | TypeArrow Type Type
        deriving Show

type Name = String

type Unifier = [(Name, Type)]

typeint :: Parser Type
typeint = do
        n <- digit
        return(TypeInt)

typevar :: Parser Type
typevar = do
        name <- many1 lower
        return (TypeVar name)

typearrow :: Parser Type
typearrow = do
        char '('
        t1 <- typee 
        char ','
        t2 <- typee 
        char ')'
        return(TypeArrow t1 t2)

typee :: Parser Type
typee =
        try typearrow <|> typevar <|> typeint

--Para sua implementação, duas funções auxiliares são necessárias, que respectivamente verificam se uma variável aparece livre em um tipo, e que compõe duas unificações distintas.
occursCheck :: Name -> Type -> Bool
occursCheck x (TypeVar y) = 
        False
--occursCheck x 
--occursCheck x

--compose :: Unifier -> Unifier -> Unifier

{--O programa deve então solicitar ao usuário dois tipos e informar se tais tipos podem ser unificados, 
informando qual o unificador mais geral, ou se a unificação não pode ser feita. Para tal, recomenda-se a 
utilização da biblioteca Parsec para a implementação de um simples parser para interpretar os tipos de 
entrada.--}

--A estrutura principal do programa se dá através da função main:
main :: IO ()
main = do        
        a <- getLine
        case parse typee "<stdin>" a of
                Right typee ->
                        print typee
                _->
                        putStrLn "erro"




{--
--A função principal para unificação receberá dois tipos e tentará retornar um unificador mais geral na forma de Just mgu, retornando Nothing caso não seja possível unificar.
unify :: Type -> Type -> Maybe Unifier

--Para sua implementação, duas funções auxiliares são necessárias, que respectivamente verificam se uma variável aparece livre em um tipo, e que compõe duas unificações distintas.
occursCheck :: Name -> Type -> Bool
compose :: Unifier -> Unifier -> Unifier

--A fim de se testar o sistema, uma função deve ser implementada capaz de aplicar uma substituição a um tipo arbitrário, retornando um novo tipo.
subst :: Unifier -> Type -> Type

--Considere também as seguintes funções para o reconhecimento de texto, definindo a seguinte gramática para tipos:
parseType :: Parser Type     -- type: function | atom
parseAtom :: Parser Type     -- atom: int | var | paren
parseInt :: Parser Type      -- int: "Int"
parseVar :: Parser Type      -- var: lowercase+
parseFun :: Parser Type      -- fun: atom "->" type
parseParen :: Parser Type    -- paren: "(" type ")"
--}