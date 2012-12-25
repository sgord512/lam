module Lambda.Language where

import Data.Set ( Set )
import qualified Data.Set as Set

import Util.Numeric ( subscriptNumber, superscriptNumber )

lambda :: String
lambda = '\x03BB':[]

data Delimiter = SquareBracket
               | Parenthesis 
               | CurlyBracket 
               deriving (Eq, Ord, Show)
                        
next :: Delimiter -> Delimiter                        
next SquareBracket = Parenthesis
next Parenthesis = CurlyBracket
next CurlyBracket = SquareBracket

surround :: Delimiter -> String -> String
surround SquareBracket str = "[" ++ str ++ "]"
surround Parenthesis str = "(" ++ str ++ ")" 
surround CurlyBracket str = "{" ++ str ++ "}"
                           
data Var = Var Char (Maybe Subscript) deriving (Eq, Ord)

data Subscript = Subscript Integer deriving (Eq, Ord)

instance Show Subscript where
  show (Subscript n) = subscriptNumber n

instance Show Var where
  show (Var c sub) = (c:[]) ++ (maybe "" show sub)

data Term = AppC Term Term | LamC Var Term | IdC Var
data Value = LamV Var Term
type FV = Set Var
type BV = Set Var

instance Show Term where
  show = pretty

freeVars :: Term -> FV
freeVars t = Set.fromList $ fv Set.empty t
  where fv :: BV -> Term -> [Var]
        fv bv (IdC v) = if v `Set.member` bv then [] else [v]
        fv bv (AppC f a) = (fv bv f) ++ (fv bv a) 
        fv bv (LamC v b) = (fv (Set.insert v bv) b)
        
        
-- Make sure that renamed var isn't bound in any of the lambda terms
renameFreeVar :: Var -> Var -> Term -> Term        
renameFreeVar orig new (IdC v) = (IdC $ if v == orig then new else v)
renameFreeVar orig new (AppC f a) = let renamer = renameFreeVar orig new 
                                   in AppC (renamer f) (renamer a)
renameFreeVar orig new term@(LamC v b) = if v == new 
                                        then let v' = firstUnusedSubscript v $ freeVars b
                                             in LamC v' (renameFreeVar orig new $ renameFreeVar v v' b)
                                        else if v == orig 
                                             then term
                                             else renameFreeVar orig new b

firstUnusedSubscript :: Var -> FV -> Var
firstUnusedSubscript (Var c sub) fv = let usedSubscripts = Set.map (\(Var _ sub') -> sub') $ Set.filter (\(Var c' sub') -> c == c') fv
                                          testSubscripts = Nothing : [Just $ Subscript n | n <- [1..]]
                                      in Var c $ head $ filter (`Set.notMember` usedSubscripts) testSubscripts
                                                  
subst :: Term -> Var -> Term -> Term
subst t var (AppC f a) = AppC (subst t var f) (subst t var a)
subst t var term@(IdC v) = if v == var then t else term
subst t var term@(LamC v b) = if var == v 
                             then term
                             else if v `Set.notMember` (freeVars t)
                                  then LamC v $ subst t var b
                                  else let v' = firstUnusedSubscript v $ Set.union (freeVars t) (freeVars b)
                                       in LamC v' $ subst t var $ renameFreeVar v v' b                             
                                          
pretty :: Term -> String
pretty term = prettyD Parenthesis term
  where prettyD :: Delimiter -> Term -> String
        prettyD d (AppC f a) = surround d $ (prettyD (next d) f) ++ " " ++ (prettyD (next d) a)
        prettyD d (LamC v b) = surround d $ lambda ++ (show v) ++ "." ++ (prettyD (next d) b)
        prettyD d (IdC v) = show v
        
pp = putStrLn . pretty

idV c = IdC $ Var c Nothing
v c = Var c Nothing