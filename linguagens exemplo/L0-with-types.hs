module Main where

import Data.IORef
import Text.ParserCombinators.Parsec
import Graphics.UI.Gtk

------- INSTRUÇÕES DE COMPILAÇÃO (Ubuntu/Linux) -------

{-

Instale as bibliotecas necessárias com o seguinte comando:

  sudo apt-get install libghc-gtk-dev libghc-parsec2-dev

Para interpretar, use

  ghci L0.hs   (dentro do interpretador chame a função "main" para iniciar o programa)

Para compilar, use

  ghc --make L0.hs

-}


-------- Tipo de Dado --------

data Term = Zero
          | Succ Term
          | T
          | F   
          | IsZero Term  
          | Pred Term  
          | If Term Term Term  
     deriving (Eq,Ord,Read)       
              


---- Conversão para String
              
instance Show Term where
  show Zero     = "0"
  show (Succ x) = "(succ "++(show x)++")" 
  show T        = "true"
  show F        = "false"
  show (IsZero x) = "(isZero "++(show x)++")"
  show (Pred x)   = "(pred "++(show x)++")"
  show (If x y z) = "(if "++(show x)++" then "++(show y)++" else "++(show z)++")"
  
  

---- Parser

termP :: Parser Term
termP =  (do string "0"
             spaces
             return Zero)
     <|>   
         (do string "succ"
             spaces
             t <- termP
             spaces
             return $ Succ t)
     <|>
         (do string "true"
             spaces
             return $ T)
     <|>
         (do string "false"
             spaces
             return $ F)
     <|> (try  -- required because 'i' is consumed and affects "if"
         (do string "isZero"
             spaces
             t <- termP
             spaces
             return $ IsZero t) )
     <|> 
         (do string "pred"
             spaces
             t <- termP
             spaces
             return $ Pred t)
     <|> 
         (do string "if"
             spaces
             t1 <- termP
             spaces
             string "then"
             spaces
             t2 <- termP
             spaces
             string "else"
             spaces
             t3 <- termP
             spaces
             return $ If t1 t2 t3)
     <|> (do string "("    
             spaces
             t <- termP
             spaces
             string ")"
             spaces
             return $ t)
         

idP :: Parser String
idP = do x <-      (letter <|> char '_')
         y <- many (letter <|> digit <|> char '_')
         return (x:y) -- identificadores


progP :: Parser Term
progP = do spaces
           t <- termP
           eof
           return t


parseTerm s = case (parse progP "" s) of
                Left _ -> Nothing
                Right t -> Just t




---- Semântica operacional

nv :: Term -> Bool  -- testa se termo é valor numérico
nv Zero     = True
nv (Succ x) = nv x
nv _        = False



value :: Term -> Bool  -- testa se termo é valor
value T = True
value F = True
value x | nv x      = True
        | otherwise = False



step :: Term -> Maybe Term  -- executa um passo da avaliação do programa
step Zero = Nothing
step (Succ x) 
  | value x   = Nothing
  | otherwise = case step x of
                  Nothing -> Nothing
                  Just x' -> Just $ Succ x'
step T    = Nothing                               
step F    = Nothing
step (IsZero Zero)                = Just T                
step (IsZero (Succ x)) | nv x     = Just F
                       | value x  = Nothing       
step (IsZero x) = case step x of
                    Nothing -> Nothing
                    Just x' -> Just $ IsZero x'
step (Pred Zero)               = Just Zero                   
step (Pred (Succ x)) | nv x    = Just x
                     | value x = Nothing
step (Pred x) = case step x of
                  Nothing -> Nothing
                  Just x' -> Just $ Pred x'
step (If T t2 t3) = Just t2
step (If F t2 t3) = Just t3
step (If t1 t2 t3) = case step t1 of
                      Nothing -> Nothing
                      Just t1' -> Just $ If t1' t2 t3




-- Sistema de Tipos
                      
data Type = Nat | Bool 
     deriving (Eq,Ord,Read,Show)
              
              
typeInference :: Term -> Maybe Type
typeInference Zero     = Just Nat
typeInference (Succ x) = case typeInference x of
                          Just Nat -> Just Nat
                          _        -> Nothing 
typeInference T   = Just Bool              
typeInference F   = Just Bool
typeInference (If t1 t2 t3) = case (typeInference t1, typeInference t2, typeInference t3) of
                                (Just Bool, Just a, Just b) | a == b  -> Just a
                                _                                     -> Nothing
typeInference (IsZero t) = case (typeInference t) of 
                            Just Nat -> Just Bool
                            _        -> Nothing
typeInference (Pred t) = case (typeInference t) of 
                            Just Nat -> Just Nat
                            _        -> Nothing
  




main = do 
  
  
  
  ----------- INTERFACE ------------
  
  initGUI
  
  -- Cria janela principal
  win <- windowNew
  set win [ windowTitle:="Simulador - Linguagem de Termos",
                  windowDefaultWidth:=640,
                  windowDefaultHeight:=480 ]
  onDestroy win mainQuit  
  
  -- Cria VBox principal
  vbox <- vBoxNew False 0
  containerAdd win vbox
  
  -- Cria Label 1
  l1 <- labelNew $ Just "Programa"
  boxPackStart vbox l1 PackNatural 0
  
  -- Cria Painel de programa
  sw1 <- scrolledWindowNew Nothing Nothing
  tv1 <- textViewNew 
  buff1 <- textViewGetBuffer tv1
  containerAdd sw1 tv1
  boxPackStart vbox sw1 PackGrow 0
  
  -- Define fontes da janela de código
  dafont <- fontDescriptionFromString "mono 12"
  widgetModifyFont tv1 (Just dafont)

  
  -- Cria Painel de botoes 1
  hbox1 <- hBoxNew False 0
  boxPackStart vbox hbox1 PackNatural 0
  
  b1 <- buttonNewWithLabel "Carrega Programa"
  boxPackEnd hbox1 b1 PackNatural 0
     

  -- Cria Label 2
  l2 <- labelNew $ Just "Execução"
  boxPackStart vbox l2 PackNatural 0

  
  -- Cria Painel de execução 
  sw2 <- scrolledWindowNew Nothing Nothing
  tv2 <- textViewNew 
  buff2 <- textViewGetBuffer tv2
  containerAdd sw2 tv2
  boxPackStart vbox sw2 PackGrow 0
  
-- Define fontes do painel de execução
  dafont <- fontDescriptionFromString "mono 12"
  widgetModifyFont tv2 (Just dafont)



  -- Cria Painel de botoes 2
  hbox2 <- hBoxNew False 0
  boxPackStart vbox hbox2 PackNatural 0

  b2 <- buttonNewWithLabel "  >  "
  boxPackEnd hbox2 b2 PackNatural 0
  
  b3 <- buttonNewWithLabel "  <  "
  boxPackEnd hbox2 b3 PackNatural 0
  
  b4 <- buttonNewWithLabel "Verifica Tipo"
  boxPackEnd hbox2 b4 PackNatural 0


  --------- ESTADO -----------

  state <- newIORef ([] :: [Term])
  
  
  --------- TRATADORES DE EVENTOS --------

  b1 `on` buttonActivated $ do      -- carrega programa
    s <- get buff1 textBufferText
    case parseTerm s of
      Nothing -> do set buff2 [textBufferText:="Expressão mal-formada"]
                    writeIORef state []
      Just t  -> do set buff2 [textBufferText:=show t]
                    writeIORef state [t]


  b2 `on` buttonActivated $ do     -- passo à frente
    p <- readIORef state
    case p of
      []    -> set buff2 [textBufferText:="Não há programa carregado!"] 
      (h:t) -> case step h of
                 Nothing -> set buff2 [textBufferText:=show h ++ "\n\n * Forma normal!"]
                 Just h' -> do 
                            modifyIORef state (h':)
                            set buff2 [textBufferText:=show h']


  b3 `on` buttonActivated $ do     -- passo atrás 
    p <- readIORef state
    case p of
      []    -> set buff2 [textBufferText:="Não há programa carregado!"] 
      (h:[])   -> set buff2 [textBufferText:=show h] 
      (h:h':t) -> do writeIORef state (h':t)
                     set buff2 [textBufferText:=show h']
  
  
  b4 `on` buttonActivated $ do     -- verifica tipo
    p <- readIORef state
    case p of
      []    -> set buff2 [textBufferText:="Não há programa carregado!"] 
      (h:t) -> case typeInference h of
                 Nothing -> set buff2 [textBufferText:=show h ++ "\n\n * Termo mal-tipado"] 
                 Just tp -> set buff2 [textBufferText:=show h ++ "\n : " ++ show tp]               


  widgetShowAll win
  mainGUI
  
  