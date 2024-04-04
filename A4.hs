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

unwrap :: Either String a -> a
unwrap e = case e of Left m -> error m; Right a -> a

unwrapTyEqs :: Either String LambdaTypeEqns -> LambdaTypeEqns
unwrapTyEqs ts = case ts of
        Left err    -> error err
        Right t     -> t

unwrapTyTerm :: Either String LambdaTyTerm -> String
unwrapTyTerm w = case w of
        Left s -> s
        Right a -> lambdaTyPrettyShow a

test1 = Abs "x" (Var "x")
tyeq1 = unwrapTyEqs $ runGetTypeEqns test1
sol1 = autoGraderTypeInference test1

test2 = App test1 test1
tyeq2 = unwrapTyEqs $ runGetTypeEqns test2
sol2 = autoGraderTypeInference test2

test3 = Abs "x" (PCase (Var "x") (("x","y"), Pair (Var "y") (Var "x")))
tyeq3 = unwrapTyEqs $ runGetTypeEqns test3
stye3 = unwrap (solveTypeEqns tyeq3)
sol3 = autoGraderTypeInference test3

test4 = App Succ (App Succ (App Succ Zero))
tyeq4 = unwrapTyEqs $ runGetTypeEqns test4
stye4 = unwrap (solveTypeEqns tyeq4)
sol4 = autoGraderTypeInference test4

test5 = FFix (Abs "x" (Var "x"))
tyeq5 = unwrapTyEqs $ runGetTypeEqns test5
sol5 = autoGraderTypeInference test5

test6  = Abs "x" (App (Var "x") (Var "x"))
tyeq6 = unwrapTyEqs $ runGetTypeEqns test6
stye6 = unwrap (solveTypeEqns tyeq6)
sol6 = autoGraderTypeInference test6

test7 = unsafeParseTerm givenExample0
tyeq7 = unwrapTyEqs $ runGetTypeEqns test7
stye7 = unwrap (solveTypeEqns tyeq7)
sol7 = autoGraderTypeInference test7

test8 = Abs "v" (Abs "f" (App Cons (Pair (Var "v")( Var "f"))))
tyeq8 = unwrapTyEqs $ runGetTypeEqns test8
stye8 = unwrap (solveTypeEqns tyeq8)
sol8 = autoGraderTypeInference test8

test9 = App (App (App (App (FFix (Abs "d" (Var "d"))) (App (Abs "y" (Pair Nil (Var "y"))) (Pair (Pair (Abs "z" (Var "z")) (Abs "a" (Var "a"))) (App (App (Abs "x" (Var "x")) (Abs "y" (Var "y"))) (App (Abs "z" (App Cons (Var "z"))) (FFix (App (Abs "c" (Var "c")) (Abs "d" (Var "d"))))))))) (Pair Unit Succ)) (App (App (Abs "e" (Var "e")) (Abs "b" (Var "b"))) (FFix (Abs "a" (App (Abs "e" (Var "e")) (Var "a")))))) (NCase Zero Zero ("c",Var "c"))
tyeq9 = unwrapTyEqs $ runGetTypeEqns test9
stye9 = unwrap (solveTypeEqns tyeq9)
sol9 = autoGraderTypeInference test9

test10 = App (App (App (App (Abs "b" (App (Var "b") (App (Abs "b" (Var "b")) Unit))) (FFix (App (Abs "e" (Var "e")) (Abs "c" (Var "c"))))) (Pair (App (Abs "x" (Var "x")) (Abs "c" (Abs "e" (LCase (Var "e") (Var "c") ("y",Var "y"))))) (Abs "a" (App Cons (Var "a"))))) (App (Abs "z" (Var "z")) (Pair (App (Abs "a" (Var "a")) Nil) (Pair (App Succ Zero) (Abs "d" (Var "d")))))) (Pair (Abs "z" (Var "z")) (Abs "y" (Var "y")))
tyeq10 = unwrapTyEqs $ runGetTypeEqns test10
stye10 = unwrap (solveTypeEqns tyeq10)
sol10 = autoGraderTypeInference test10

test11 = App (App (App (App (App (App (Abs "y" (Var "y")) (Abs "c" (App (Var "c") Unit))) (App (Abs "d" (Var "d")) (Abs "e" (FFix (UCase (Var "e") (Abs "a" (Var "a"))))))) (App Succ Zero)) (Pair (Abs "z" (App (Abs "d" (Var "d")) (Var "z"))) (Abs "x" (LCase Nil (Var "x") ("d",Var "d"))))) (Pair (Abs "b" (Var "b")) (Abs "b" (Var "b")))) (App (App (App (Abs "e" (Var "e")) (Abs "x" (Var "x"))) (Abs "c" (Var "c"))) (Pair Cons Cons))
tyeq11 = unwrapTyEqs $ runGetTypeEqns test11
stye11 = unwrap (solveTypeEqns tyeq11)
sol11 = autoGraderTypeInference test11

test12 = App (App (App (App (FFix (Abs "x" (Var "x"))) (Pair Zero (Abs "x" (Var "x")))) (Pair (Abs "c" (Abs "a" (LCase (Var "a") (Var "c") ("d",Abs "y" (LCase (Var "y") (Var "d") ("y",Var "y")))))) (App (App (Abs "b" (Var "b")) (Abs "a" (Var "a"))) (Pair (App (Abs "z" (LCase (Var "z") Nil ("b",App Cons (Var "b")))) (FFix (Abs "y" (Var "y")))) (App (Abs "c" (Var "c")) (Abs "z" (Var "z"))))))) (Pair Succ (Abs "e" (Var "e")))) (App (App (Abs "e" (Var "e")) (Abs "e" (Var "e"))) (Pair Unit (Abs "d" (Var "d"))))
tyeq12 = unwrapTyEqs $ runGetTypeEqns test12
stye12 = unwrap (solveTypeEqns tyeq12)
sol12 = autoGraderTypeInference test12

test13 = App (Abs "x" (Var "x")) (Abs "n" (UCase (Var "n") Unit))
tyeq13 = unwrapTyEqs $ runGetTypeEqns test13
stye13 = unwrap (solveTypeEqns tyeq13)
sol13 = autoGraderTypeInference test13