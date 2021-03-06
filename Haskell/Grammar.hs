module Grammar (SchVal(..), 
               env, Nil, Store, Env)  where
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State as S

type Nil = []
type Store = (Map.Map String SchVal)
type Env = S.State (Map.Map String SchVal) SchVal 

data SchVal =
    NumVal Integer
   | Func ([SchVal] -> SchVal)
   | Lambda { params :: SchVal , body :: SchVal  }
   | BoolVal Bool
   | StringVal String
   | Symbol String
   | List [SchVal]
   | Define SchVal SchVal
   | Let [SchVal] SchVal
   | LetBind (SchVal, SchVal)
   | If SchVal SchVal SchVal
   | Quote SchVal
   | NoVal
   
instance Show SchVal where
   show (NumVal x)    = show x
   show (Func x)      = show "function"
   show (BoolVal x)   = show x
   show (StringVal x) = "\"" ++ x ++ "\""
   show (List x)      = show x
   show (Symbol x)    = x
   show (Lambda x y) =  "lambda" ++ (show x) ++ (show $ y)
   show NoVal         = "; no values returned"

instance Eq SchVal where
  (==) (NumVal x) (NumVal y) = x == y
  (==) (StringVal x) (StringVal y) = x == y
  (==) (List x) (List y) = x == y
  (==) (BoolVal x) (BoolVal y) = x == y

--'!=' is not the right operator
env = Map.fromList [("+", plus), ("-", sub),
                    ("*", mul), ("/", divide),
                    (">", gt), (">=", ge),
                    ("<", lt), ("<=", le),
                    ("eq?", eq), ("!=", neq),
                    ("list", makeList), ("cdr", cdr),
                    ("car", car), ("cons", cons),
                    ("nil", List [])]
                    --("lambda", (StringVal "Lam"))] 

getArithOp :: (Integer -> Integer -> Integer) -> SchVal
getArithOp f = (Func  (\ (x:xs) ->  foldl (liftArithOp f) x xs)) 

getCompOp :: (Integer -> Integer -> Bool) -> SchVal
getCompOp f    = (Func  (\ (x:xs) ->
                          BoolVal $ all ( (liftBoolOp f) x ) xs ) )

liftArithOp :: (Integer -> Integer -> Integer) -> SchVal -> SchVal -> SchVal
liftArithOp f (NumVal x) (NumVal y) = NumVal $ f x y

liftBoolOp :: (Integer -> Integer -> Bool) -> SchVal -> SchVal -> Bool
liftBoolOp f (NumVal x) (NumVal y) = f x y

cons :: SchVal
cons = (Func (\ [x, (List ls)] -> (List (x:ls))))

cdr :: SchVal
cdr = (Func (\ ((List ( x:xs)):[]) -> (List xs) ))

car :: SchVal
car = (Func (\ ((List ( x:xs)):[]) -> x))

-- nil :: SchVal
-- nil = (Func (\ (List []) -> BoolVal True))

makeList :: SchVal
makeList = (Func (\ args -> (List args)))

plus :: SchVal --[SchVal] -> SchVal
plus = getArithOp (+) 

mul :: SchVal
mul = getArithOp (*) 

divide :: SchVal
divide = getArithOp (div) 

sub :: SchVal
sub = getArithOp (-) 

gt :: SchVal
gt = getCompOp (>)

ge :: SchVal
ge = getCompOp (>=)

lt :: SchVal 
lt = getCompOp (<)

le :: SchVal
le = getCompOp (<=)

eq :: SchVal
eq = (Func (\ (x:xs) ->
             BoolVal $ all ( (==) x) xs))

neq :: SchVal
neq = getCompOp (/=)

