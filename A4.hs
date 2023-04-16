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

test1 = Abs "x" (Var "x")
tyeq1 = unwrapTyEqs $ runGetTypeEqns test1
sol1 = solveTypeEqns tyeq1

test2 = App test1 test1
tyeq2 = unwrapTyEqs $ runGetTypeEqns test2
sol2 = solveTypeEqns tyeq2

test3 = Abs "x" (PCase (Var "x") (("x","y"), Pair (Var "y") (Var "x")))
tyeq3 = unwrapTyEqs $ runGetTypeEqns test3
sol3 = solveTypeEqns tyeq3

test4 = App Succ (App Succ (App Succ Zero))
tyeq4 = unwrapTyEqs $ runGetTypeEqns test4
sol4 = solveTypeEqns tyeq4