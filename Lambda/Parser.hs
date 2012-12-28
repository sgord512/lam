module Lambda.Parser where

import Control.Applicative hiding ( (<|>), optional )

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Prim
import Text.Parsec.String

import Lambda.Language

termP :: Parser Term
termP =   try (parens $ (try lamCP) <|> appCP)
      <|> try lamCP
      <|> appCP          
      <|> IdC <$> varP <* spaces                   
      <?> "term"
      
parens = between (char '(') (char ')')
      
termP' :: Parser Term         
termP' =   (try $ parens $ try lamCP <|> appCP)
       <|> IdC <$> varP
       <?> "term'"
         
appCP = foldl1 AppC <$> termP' `sepBy1` (space *> spaces) <?> "application"

lamCP =   LamC <$> (char '\\' *> spaces *> varP) <* (spaces <* char '.') <*> (space *> spaces *> termP) 
      <?> "abstraction"
      
varP :: Parser Var
varP =   Var <$> letter <*> optionMaybe (char '_' *> subscriptP)
     <?> "variable"
             
subscriptP :: Parser Subscript             
subscriptP =   Subscript . read <$> many1 digit 
           <?> "subscript"