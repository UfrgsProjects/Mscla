
import System.Environment
import Data.IORef
import Text.ParserCombinators.Parsec
--import Graphics.UI.Gtk
import Control.Monad

-- Uma variavel, é um tipo assoc a um nome
data MsclaVar = Var String MsclaType

instance Show MsclaVar where
  show (Var aName aType) = aName++":"++(show aType)

-- Tipos do sistema de tipos
data MsclaType = MsclaInt
               | MsclaBool

instance Show MsclaType where
  show (MsclaInt) = "int"
  show (MsclaBool) = "bool"


-- Conjunto de operacoes
data Op = MsclaPlus 
        | MsclaMinus
        | MsclaLeq
          
instance Show Op where
  show MsclaPlus = "+"
  show MsclaMinus = "-"
  

-- Gramatica de termos
data Term = Number Integer
          | Fun MsclaVar Term
          | If Term Term Term
          | T 
          | F
          | MsclaOp Op Term Term
          | Let MsclaVar Term
          | App Term Term
          | BoundVar String          
            
instance Show Term where
  show (Number x) = (show x)
  show (If x y z) = "(if "++(show x)++" then "++(show y)++" else "++(show z)++")"
  show (Fun x y) = "fn "++(show x)++" => "++(show y)
  show (MsclaOp op term1 term2) = ((show term1)++" "++(show op)++" "++(show term2)++"oi")
  show (Let aVar aTerm) = "let "++(show aVar)++" = "++(show aTerm)
  show T = "true"
  show F = "false"
  

-- *** FUNCOES DE TESTE DOS TIPOS CRIADOS *** --
makeMsclaInt :: Integer -> Term
makeMsclaInt x =  Number x

makeMsclaType :: String -> MsclaType
makeMsclaType x = case x of
  "int" -> MsclaInt
  _ -> MsclaBool

makeMsclaVar :: String->String->MsclaVar
makeMsclaVar x y = Var y (makeMsclaType x) 

makeMsclaFun :: String->String->Term->Term
makeMsclaFun x y z = Fun (makeMsclaVar x y) z

makeSum :: Integer -> Term
makeSum x = MsclaOp MsclaPlus (makeMsclaInt x) (makeMsclaInt x)




parseNumber :: Parser Term
parseNumber = liftM (Number . read) $ many1 digit

parseVar :: Parser MsclaVar
parseVar = do  theName <- many (letter <|> digit)
               string ":"
               theType <- many (letter <|> digit)
               return $ Var theName MsclaInt
               

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

parseOp :: Parser Op
parseOp = do theSymbol <- many (symbol)
             return $ case theSymbol of 
                        "+" -> MsclaPlus
                        "-" -> MsclaMinus
                        
              
parseExpr :: Parser Term
parseExpr = parseNumber
            <|>
            (do string "if"
                spaces
                t1 <- parseExpr
                spaces
                string "then"
                spaces
                t2 <- parseExpr
                spaces
                string "else"
                spaces
                t3 <- parseExpr
                spaces
                return $ If t1 t2 t3)
            <|>
            (do string "true"
                spaces
                return $ T)
            <|>
            (do string "false"
                spaces
                return $ F)
            <|>
            (do string "let"
                spaces
                t1 <- parseVar
                spaces
                string "="
                spaces
                t2 <- parseExpr
                spaces
                return $ Let t1 t2)
            <|>
            (do string "xn" --nao posso colocar nenhuma letra tipo f ou l
                spaces
                t1 <- parseVar
                spaces
                string "=>"
                spaces
                t2 <- parseExpr
                spaces
                return $ Fun t1 t2)
            <|>
            (do string "("    
                spaces
                t <- parseExpr
                spaces
                string ")"
                spaces
                return $ t)
            <|>
            (do op <- parseOp
                spaces
                t1 <- parseExpr
                spaces
                t2 <- parseExpr
                spaces
                return $ MsclaOp op t1 t2)
						<|>
            (do theName <- many (letter <|> digit)  -- NAO COMPILA
						    return $ BoundVar theName )

              
readExpr :: String -> String
readExpr input = case parse parseExpr "mscla" input of
    Left err -> "No match: " ++ show err
    Right t -> show t

readOp :: String -> String
readOp input = case parse parseOp "msclaOP" input of
    Left err -> "No match: " ++ show err
    Right t -> show t
    
{-
main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))
-}

{-
main :: IO ()
main = putStrLn $ show $ readExpr ("+ 1 1")
-}



main :: IO ()
main = putStrLn $ show $ readExpr "xn x:int => + x 1"


{-
main :: IO ()
main = putStrLn $ show $ readOp "1 + 1"
-}


nv :: Term -> Bool
nv (Number _) = True
nv _ = False

value :: Term -> Bool  -- testa se termo é valor
value T = True
value F = True
value x | nv x      = True
        | otherwise = False


-- Esqueleto, fazer também op's booleanas == > <
doOperation :: Op->Term->Term->Term
doOperation theOp (Number t1) (Number t2) = case theOp of
  MsclaPlus  -> Number $ t1 + t2
  MsclaMinus -> Number $ t1 - t2
  
isInContext :: [MsclaVar]->Term->Bool
isInContext [] _ = False
isInContext ((Var theName theType):xs) (BoundVar theBoundVarName) = case theName == theBoundVarName of
                                                                  True  -> True
                                                                  False -> isInContext xs (BoundVar theBoundVarName)

-- Tentando fazer currying, substitui apenas o primeiro termo explicitado na "lista" de contexto
-- TODO pensar em funcoes
subs :: Term->Term->[MsclaVar]->Term
subs _ (Number n) _ = Number n
subs _ T _ = T
subs _ F _ = F
subs theTerm (MsclaOp theOp term1 term2) context = MsclaOp theOp (subs theTerm term1 context) (subs theTerm term2 context)
subs theTerm (If term1 term2 term3) c = If (subs theTerm term1 c) (subs theTerm term2 c) (subs theTerm term3 c)
subs theTerm (Let letVar term) c = Let letVar $ subs theTerm term c
subs theTerm (BoundVar varName) ((Var cVarName _ ):xs) = case varName == cVarName of -- Cheguei no termo x e x é o primeiro arg
																											 True  -> theTerm
																											 False -> (BoundVar varName)
 
  
step :: Term -> Maybe Term  -- executa um passo da avaliação do programa
step T = Nothing
step F = Nothing
step (Number _) = Nothing
step (MsclaOp op term1 term2)
  | nv term1 && nv term2 = Just $ doOperation op term1 term2
  | nv term1 = step term2
  | otherwise = step term1
step (If T t2 t3) = Just t2
step (If F t2 t3) = Just t3
step (If t1 t2 t3) = case step t1 of
                      Nothing -> Nothing
                      Just t1' -> Just $ If t1' t2 t3

