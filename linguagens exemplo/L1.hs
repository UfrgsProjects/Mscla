
import Data.IORef
import Text.ParserCombinators.Parsec
import Graphics.UI.Gtk

-- Uma variavel, é um tipo assoc a um nome
data MsclaVar = Var MsclaType String

instance Show MsclaVar where
  show (Var aType aName) = (show aType)++":"++aName

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
          | Bool 
          | MsclaOp Op Term Term
          | Let MsclaVar Term
          | App Term Term
         
            
instance Show Term where
  show (Number x) = (show x)
  show (If x y z) = "(if "++(show x)++" then "++(show y)++" else "++(show z)++")"
  show (Fun x y) = "fn "++(show x)++" => "++(show y)
  show (MsclaOp op term1 term2) = (show term1)++" "++(show op)++" "++(show term2)
  

-- *** FUNCOES DE TESTE DOS TIPOS CRIADOS *** --
makeMsclaInt :: Integer -> Term
makeMsclaInt x =  Number x

makeMsclaType :: String -> MsclaType
makeMsclaType x = case x of
  "int" -> MsclaInt
  _ -> MsclaBool

makeMsclaVar :: String->String->MsclaVar
makeMsclaVar x y = Var (makeMsclaType x) y

makeMsclaFun :: String->String->Term->Term
makeMsclaFun x y z = Fun (makeMsclaVar x y) z

makeSum :: Integer -> Term
makeSum x = MsclaOp MsclaPlus (makeMsclaInt x) (makeMsclaInt x)

