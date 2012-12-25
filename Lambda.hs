module Lambda where 

import Data.Map ( Map )
import qualified Data.Map as Map

import Lambda.Language

step :: Term -> Term
step (AppC f a) = case f of
  (LamC v b) -> subst a v b
  (AppC f' a') -> AppC (step f) a  
  _ -> error "Can't apply a non-function"
step (IdC v) = error $ "Unbound identifier: " ++ (show v)
step term@(LamC v b) = error "Already fully reduced"
     
normalForm :: Term -> Term
normalForm t = case step t of
  (LamC v b) -> (LamC v b)
  t' -> normalForm t'
  
alphaEquivalent :: Term -> Term -> Bool
alphaEquivalent (AppC f a) (AppC f' a') = (alphaEquivalent f f') && (alphaEquivalent a a')
alphaEquivalent (LamC v b) (LamC v' b') = 
  if v == v' 
  then alphaEquivalent b b'
  else alphaEquivalent b $ subst (IdC v) v' b' 
alphaEquivalent (IdC v) (IdC v') = v == v'
alphaEquivalent _ _ = False
  
                       
combinators = Map.fromList [("y", let half = LamC (v 'x') $ AppC (idV 'f') $ AppC (idV 'x') (idV 'x') 
                                  in LamC (v 'f') $ AppC half half),
                            ("omega", let f = LamC (v 'x') $ AppC (idV 'x') (idV 'x') 
                                      in AppC f f)]
                       
combinator :: String -> Term
combinator str = (Map.!) combinators str 

churchNumeral :: Integer -> Term
churchNumeral n = LamC (v 'f') (LamC (v 'x') $ cn n)
  where cn :: Integer -> Term
        cn 0 = (idV 'x')
        cn n = AppC (idV 'f') $ cn (n - 1)

