module Lambda.Parser where

import Control.Applicative hiding ( (<|>), optional )

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Prim
import Text.Parsec.String

import Lambda.Language

termP :: Parser Term
termP =   between (char '(') (char ')') (AppC <$> termP <*> (space *> spaces *> termP))
      <|> LamC <$> (char '\\' *> spaces *> varP) <* char '.' <*> termP
      <|> IdC <$> varP
      <?> "term"
      
varP :: Parser Var
varP = Var <$> letter <*> optionMaybe (char '_' *> subscriptP)
       <?> "variable"
             
subscriptP :: Parser Subscript             
subscriptP = Subscript . read <$> many1 digit