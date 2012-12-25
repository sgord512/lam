module Tests where

import Test.HUnit
import Lambda
import Lambda.Language

tests = test [ "alphaEquivalence" ~: alphaEquivalent (LamC (v 'f') (idV 'f')) (LamC (v 'g') (idV 'g')) ~=? True, 
               "omega" ~: alphaEquivalent (step $ combinator "omega") (combinator "omega") ~=? True]
                             
