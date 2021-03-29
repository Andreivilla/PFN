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


{--
main :: IO ()
main = do        
        a <- getLine
        case parse typee "<stdin>" a of
                Right typee ->
                        print typee
                _->
                        putStrLn "erro"
--}
--A estrutura principal do programa se dá através da função main:
main :: IO ()
main = do
        putStrLn "Digite um termo:"
        a <- getLine
        let Right ta = parse typee "<stdin>" a
        
        --
        putStrLn "Digite outro termo:"

        -- Bug do repl.it! Lê uma linha extra...
        --getLine -- Remova se compilar localmente...

        b <- getLine
        -- Assume que o parsing deu certo!
        let Right tb = parse typee "<stdin>" b
        --
        putStrLn "Unificação:"
        print ta
        print tb
        print $ unify ta tb



--A função principal para unificação receberá dois tipos e tentará retornar um unificador mais geral na forma de 
--Just mgu, retornando Nothing caso não seja possível unificar.
unify :: Type -> Type -> Maybe Unifier
unify (TypeVar a) (TypeVar b) | a == b = Just []
unify TypeInt TypeInt = Just []
--unify _ _ = Nothing

unify (TypeVar a) b = if occursCheck a b then Nothing else Just [(a, b)]
unify b (TypeVar a) = if occursCheck a b then Nothing else Just [(a, b)]
unify (TypeVar x) (TypeArrow a b) = if occursCheck x (TypeArrow a b) == True then Nothing else Just [(x,a)]


unifyarrow _ _ = [] 

--Para sua implementação, duas funções auxiliares são necessárias, que respectivamente 
--verificam se uma variável aparece livre em um tipo, e que compõe duas unificações distintas.
occursCheck :: Name -> Type -> Bool
occursCheck x (TypeInt) = False
occursCheck x (TypeVar y) = x == y
occursCheck x (TypeArrow y xs) = if occursCheck x y then True else occursCheck x xs

--compose :: Unifier -> Unifier -> Unifier
--compose x y = x



