--Scheme interpeter 
--Authors:
--Ace Levenberg (llevenbe@ucsc.edu)
--Oren Leiman (oleiman@ucsc.edu)

import System.IO
import Parser (statementP)
import Grammar (SchVal(..), env, Nil, Store, Env)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import Text.Parsec.Language (haskellDef)
import qualified Text.Parsec.Token as P
import Control.Monad.State as S

main = repl env
repl :: Store -> IO()
repl env = do
      hPutStr stdout "haskeeeee >>"
      hFlush stdout
      program <- hGetLine stdin
      (x,y) <- run program env
      hPutStr stdout $ (show x) ++ "\n"
      hFlush stdout
      repl y 

run :: String -> Store -> IO (SchVal, Store)
run program symTable = return $ runState (eval parsed) symTable
    where parsed = case (parse statementP "lisp" program) of
                     Left err  -> (StringVal (show err))
                     Right stm -> stm

eval :: SchVal -> Env
eval n@(NumVal _ )     = return n
eval b@(BoolVal _ )    = return b
eval s@(StringVal _ )  = return s
eval l@(Lambda _ _)    = return l
--WAT----------------------------------
eval (List  [l@(Lambda _ _)]) = eval l
---------------------------------------
eval (Symbol s) = do
      symTable <- get
      return (symTable Map.! s)

eval (List [(Quote list)] ) = return $ list

eval (List [(If cond expr expr')]) = do
      cond' <- eval cond
      case cond' of
        (BoolVal True)  -> eval expr
        (BoolVal False) -> eval expr'
          
eval (List [(Let binds body)]) = do
      symTable <- get
      args <- mapM eval $ snd unzipped
      return $ apply function args symTable
        where function = (Lambda (List paramlst) body)
              paramlst = fst unzipped
              unzipped = unzip $ map (\(LetBind x) -> x) binds

eval (List [(Define (List ((Symbol x):xs) ) body)] ) = do
      modify $ (\ symTable -> Map.insert x fn symTable)
      return NoVal 
          where fn = (Lambda (List xs) body)

eval (List [(Define (Symbol var) value)] ) = do
      val <- eval value
      modify $ insertvar var val
      symTable <- get
      return NoVal
          where insertvar = (\var val symTable ->
                             Map.insert var val symTable)      
eval (List (x:xs))  = do
     f <- eval x
     args <- mapM eval xs
     symTable <- get
     return $ apply f args symTable

-- new version of apply that takes a Store (our map) this is the quick
apply ::  SchVal -> [SchVal] -> Store -> SchVal
apply (Func f) args _ = f args
apply (Lambda (List params) body) args symTable = 
      evalState (eval body) newTable
          where newTable = (Map.union localTable symTable)
                localTable =  Map.fromList (zip (map ripSymbol params) args)
                ripSymbol (Symbol x) = x
                                                                                             
