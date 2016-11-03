module Parser where

import SyntaxTree
import Eval
import Text.ParserCombinators.Parsec
import Control.Monad.Error

number = do
  sign <- option "" (string "-")
  digits <- many1 digit

  try (do
        char '.'
        dec <- many1 digit
        return $ Value (SFloat (read (sign ++ digits ++ "." ++ dec))))
          <|> return (Value (SNum (read (sign++digits))))

symbol = identifier >>= return . f
    where
      f "#f" = Boolean False
      f "#t" = Boolean True
      f x = Sym x

quoted = do
  char '\''
  x <- parseExprAux
  return $ Quoted x

identifier = do
  f <- firstAllowed
  r <- many (firstAllowed <|> digit)
  return $ (f:r)
      where firstAllowed = oneOf "+-*/!=#!$%&:<>?^_~" <|> letter

whitespace = skipMany1 (space <|> newline <|> tab <|> comment)

parseExprAux :: GenParser Char st AST
parseExprAux = do
  optional (many1 comment)
  res <- parseExprAux'
  return res

parseExprAux' :: GenParser Char st AST
parseExprAux' = (skipMany whitespace >>)
               quoted
               <|> sString
               <|> (try number)
               <|> symbol
               <|> do {
                     char '(';
                     skipMany whitespace;
                     res <- (try parseImproper) <|> list ;
                     char ')';
                     return res }

comment = do
  char ';'
  manyTill anyChar (try $ (newline >> return ()) <|> eof)
  skipMany whitespace
  return ' '

list = do
  x <- (parseExprAux `sepEndBy` (many1 whitespace))
  return $ Sequence x

parseImproper = do
  head <- endBy parseExprAux spaces
  tail <- char '.' >> spaces >> parseExprAux
  return $ Improper head tail

parseExpr = do
  skipMany whitespace
  x <- parseExprAux
  skipMany whitespace
  optional eof
  return x

--parseAll :: String -> [SComputation]
parseExprAll = many1 parseExpr

parseOne :: String -> ASTComputation
parseOne str =
    case (Text.ParserCombinators.Parsec.parse parseExpr "" str) of
      Right x -> return x
      Left e -> throwError $ show e

parseAll :: String -> SComputation [AST]
parseAll str =
    case (Text.ParserCombinators.Parsec.parse parseExprAll "" str) of
      Right x -> return x
      Left e -> throwError $ show e


sString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ Str x

