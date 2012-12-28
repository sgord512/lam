module Lambda.Eval where 

import Data.Map ( Map )
import qualified Data.Map as Map

import Lambda.Language

untilFixed :: (Term -> Term) -> Term -> Term
untilFixed reducer term = 
  let term' = reducer term
  in if alphaEquivalent term term'
     then term'
     else untilFixed reducer term'
     
alphaEquivalent :: Term -> Term -> Bool
alphaEquivalent (AppC f a) (AppC f' a') = (alphaEquivalent f f') && (alphaEquivalent a a')
alphaEquivalent (LamC v b) (LamC v' b') = 
  if v == v' 
  then alphaEquivalent b b'
  else alphaEquivalent b $ subst (IdC v) v' b' 
alphaEquivalent (IdC v) (IdC v') = v == v'
alphaEquivalent _ _ = False
  
evalNormalOrder :: Term -> Term
evalNormalOrder t = untilFixed stepNormal t

stepNormal :: Term -> Term
stepNormal (AppC f a) = case f of
  (LamC v b) -> subst a v b
  (AppC f' a') -> AppC (stepNormal f) a  
  _ -> error "Can't apply a non-function"
stepNormal (IdC v) = error $ "Unbound identifier: " ++ (show v)
stepNormal term@(LamC v b) = term

combinators = Map.fromList [("y", let half = LamC (v 'x') $ AppC (idV 'f') $ AppC (idV 'x') (idV 'x') 
                                  in LamC (v 'f') $ AppC half half),
                            ("omega", let f = LamC (v 'x') $ AppC (idV 'x') (idV 'x') 
                                      in AppC f f)
                            ("z", let half = LamC (v 'x') $ AppC (idV 'f') (LamC (v 'y') (AppC (AppC (idV 'x') (id 'x')) (idV 'y')))
                                  in Lam (v 'f') $ AppC half half)]
                                     
combinator :: String -> Term
combinator str = (Map.!) combinators str 

churchNumeral :: Integer -> Term
churchNumeral n = LamC (v 'f') (LamC (v 'x') $ cn n)
  where cn :: Integer -> Term
        cn 0 = (idV 'x')
        cn n = AppC (idV 'f') $ cn (n - 1)

