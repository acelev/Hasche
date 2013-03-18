module Parser (statementP)  where
import Grammar(SchVal(..))
import Text.ParserCombinators.Parsec
import Text.Parsec.Language (emptyDef, haskellDef)
import qualified Text.Parsec.Token as P

def = emptyDef { P.reservedNames = ["quote", "if", "define", "let", "lambda", "#t", "#f"],
                 P.opStart = oneOf "*/-+<>e",
                 P.opLetter = oneOf "*/-+<>eq?="}

P.TokenParser { P.reserved = m_reserved,
                P.reservedOp = m_reservedOp,
                P.stringLiteral = m_stringLiteral,
                P.integer = m_integer,
                P.float = m_float,
                P.parens = parens,
                P.lexeme = lexeme } = P.makeTokenParser def

statementP :: Parser SchVal
statementP = symbolP <|> exprP <|> valP

exprP :: Parser SchVal
exprP = do
     x <-parens $ many e
     return (List x)
       where e = (specialP <|> opP <|> symbolP <|> valP <|>  exprP)

specialP :: Parser SchVal
specialP = defineP <|>  letP <|> ifP <|> lambdaP <|> quoteP 

defineP :: Parser SchVal
defineP = do
     m_reserved "define"
     x <- symbolP <|> exprP
     y <- statementP
     return $ (Define x y)

letP :: Parser SchVal
letP = do
      m_reserved "let"
      binds <- parens $ many1 $ parens letBindP 
      body <- statementP
      return $ Let binds body

letBindP :: Parser SchVal
letBindP = do
      symbol <- symbolP
      val <- statementP <|> opP
      return $ LetBind (symbol, val)

ifP :: Parser SchVal
ifP = do
     m_reserved "if"
     cond <- statementP
     expr1 <- statementP
     expr2 <- statementP
     return $ If cond expr1 expr2

lambdaP :: Parser SchVal
lambdaP = do
     m_reserved "lambda"
     params <- exprP
     body   <- valP <|> symbolP <|> exprP
     return $ Lambda params body

quoteP :: Parser SchVal
quoteP = do
     m_reserved "quote"
     list <- parens $ many $ (valP <|> symbolP) 
     return $ Quote (List list) 

symbolP :: Parser SchVal
symbolP = do
     x <- letter 
     y <- lexeme $ many (letter <|> digit <|> (oneOf "_?"))
     return (Symbol $ x:y)
     
valP :: Parser SchVal
valP = boolValP <|> numValP <|> stringValP

opParser :: String -> Parser SchVal
opParser str = constP str (Symbol str)

opP :: Parser SchVal
opP = (m_reservedOp "+"   >> return (Symbol "+"))
  <|> (m_reservedOp "-"   >> return (Symbol "-"))
  <|> (m_reservedOp ">="  >> return (Symbol ">="))
  <|> (m_reservedOp "*"   >> return (Symbol "*"))
  <|> (m_reservedOp "<="  >> return (Symbol "<="))
  <|> (m_reservedOp "eq?" >> return (Symbol "eq?"))
  <|> (m_reservedOp "/"   >> return (Symbol "/"))
  <|> (m_reservedOp ">"   >> return (Symbol ">"))
  <|> (m_reservedOp "<"   >> return (Symbol "<"))
  
stringValP :: Parser SchVal
stringValP = do str <- m_stringLiteral
                return (StringVal str)

numValP :: Parser SchVal
numValP = do
     x <- m_integer
     return (NumVal x)

constP :: String -> a -> Parser a
constP s x = do
     lexeme $ string s
     return x

boolValP :: Parser SchVal
boolValP = (m_reserved "#t" >> return (BoolVal True))
       <|> (m_reserved "#f" >> return (BoolVal False))
