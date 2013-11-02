
import System.Environment
import Data.IORef
import Text.ParserCombinators.Parsec
import Graphics.UI.Gtk
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

data Bool = True
          | False

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
         
            
instance Show Term where
  show (Number x) = (show x)++"tchau"
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
parseExpr = (do string "if"
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
            (do t1 <- parseExpr
                spaces
                op <- parseOp
                spaces
                t2 <- parseExpr
                spaces
                return $ MsclaOp op t1 t2)
            <|>
            	parseNumber
            
            
              
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


main :: IO ()
main = putStrLn $ show $ readExpr ("1 + 1")



{-
main :: IO ()
main = putStrLn $ show $ readExpr "xn x:int => (( if true then (if true then true else false) else true ) + 1)"
-}

{-
main :: IO ()
main = putStrLn $ show $ readOp "1 + 1"
-}
