module Parser (parseGrammar, grammar) where

import Grammar              hiding (label)
import Data.Void            (Void)
import Data.Char            (isSymbol)
import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Text.Megaparsec.Char.Lexer as Lexer
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme (Lexer.space space1 (Lexer.skipLineComment "--") empty)

nonTerm :: Parser String
nonTerm = label "non terminal" $ (:) <$> upperChar <*> many (noneOf (" =\n\r." :: String))

term :: Parser String
term = label "terminal" $ (:) <$> lowerChar <*> many (noneOf (" =\n\r." :: String))

expr :: Parser Point
expr = (Term Eof)     <$  char '$'
   <|> (Term Empty)   <$  char 'ε'
   <|> (Term . Named) <$> term
   <|> (NonTerm)      <$> nonTerm

chain :: Parser a -> Parser b -> Parser [a]
chain p op = (:) <$> p <*> many (op >> p)

clause :: Parser (Map String (Set Rule))
clause = do
        name <- lexeme nonTerm
        lexeme (string "->")
        body <- Set.singleton <$> seq name
        lexeme ((() <$ char '\n') <|> eof)
        pure (Map.singleton name body)
    where
        seq name = MkRule name <$> chain expr (some (char ' '))

grammar :: Parser Grammar
grammar = do
    start <- lexeme (string "start") *> (lexeme nonTerm)
    rules <- foldl (Map.unionWith (<>)) Map.empty <$> some (lexeme clause)
    pure (MkGrammar rules start)

parseGrammar :: String -> Either String Grammar
parseGrammar input =
  case parse (grammar <* eof)  ""  input of
    Left err -> Left $ errorBundlePretty err
    Right output -> Right output