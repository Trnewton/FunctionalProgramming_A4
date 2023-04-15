module A4 where

{- This module is for playing with functions in ghci..
 - In other words, type
 -  ``ghci A4.hs``
 - so you can play with your code from the modules imported below..
 -}

import Lib.AST
import Lib.ASTParse
import TypeInference
import Examples
import Lib.Monads

unwrapTyEqs :: Either String LambdaTypeEqns -> LambdaTypeEqns
unwrapTyEqs ts = case ts of
        Left err    -> error err
        Right t     -> t

test1 = Abs "x" (Var "x")
tyeq1 = unwrapTyEqs $ runTypeInference test1

test2 :: Term [Char] [Char]
test2 = App test1 test1
tyeq2 = unwrapTyEqs $ runTypeInference test2

test3 = Abs "x" (PCase (Var "x") (("x","y"), Pair (Var "y") (Var "x")))
tyeq3 = unwrapTyEqs $ runTypeInference test3