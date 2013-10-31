

-- Na verdade é um tipo composto, variaveis sao um nome mais um tipo
type Var = String


data Bool = True
          | False

data Op = MsclaPlus 
        | MsclaMinus
        | MsclaLeq
          
instance Show Op where
  show MsclaPlus = "+"
  show MsclaMinus = "-"
  

data Term = Number Integer
          | Fun Var Term
          | If Term Term Term
          | Bool 
          | MsclaOp Op Term Term
          | Let Var Term
         
            
instance Show Term where
  show (Number x) = (show x)
  show (If x y z) = "(if "++(show x)++" then "++(show y)++" else "++(show z)++")"
  show (Fun x y) = "fn "++(show x)++" => "++(show y)
  show (MsclaOp op term1 term2) = (show term1)++" "++(show op)++" "++(show term2)
  
makeMsclaInt :: Integer -> Term
makeMsclaInt x =  Number x

makeMsclaFun :: String->Term->Term
makeMsclaFun x y = Fun x y

makeSum :: Integer -> Term
makeSum x = MsclaOp MsclaPlus (makeMsclaInt x) (makeMsclaInt x)

