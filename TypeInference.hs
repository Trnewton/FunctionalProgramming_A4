module TypeInference where

import Lib.Monads
import Lib.AST

-- | autoGraderTypeInference
-- You should implement this function! This function should take a LambdaTerm
-- and output either:
--      - Left: an informative error message (either a free variable error,
--          or a type inferencing error such as match failure or occurs check)
--      - Right: the most general inferred type of the lambda term
-- Wahoo!! Last assignment you're nearly there :D
autoGraderTypeInference :: LambdaTerm -> Either String LambdaTyTerm
autoGraderTypeInference = undefined

data TypeEqns var
    = TypeExist [var] [TypeEqns var]
    | TypeEqn (TyTerm () var, TyTerm () var)

type LambdaTypeEqns = TypeEqns String
type Context = [(LambdaTerm, LambdaTyTerm)]

data Stack = Data
    { context :: Context
    , typeCount :: Int }

lookUpType :: LambdaTerm -> Context -> Maybe LambdaTyTerm
lookUpType t [] = Nothing
lookUpType t ((t', ty):ts)
    | t == t'   = Just ty
    | otherwise = lookUpType t ts

incVarCount :: Stack -> Stack
incVarCount st = st { typeCount = typeCount st + 1}

getNewTypeVar :: StateT Stack (Either String) LambdaTyTerm
getNewTypeVar = StateT (\st -> Right (TyVar () (show (typeCount st)), incVarCount st))

typeInference :: LambdaTerm -> StateT Stack (Either String) LambdaTypeEqns
typeInference (Var v) = undefined --do
    -- oldTyp  <- StateT (\st -> Right (lookUpType (Var v) (context st), st))
    -- newTyp  <- getNewTypeVar
    -- return (TypeEqn (oldTyp, newTyp))
typeInference (Abs abs t) = undefined
typeInference (App t1 t2) = do
    v1 <- StateT (\st -> Right (show (typeCount st), incVarCount st))
    v2 <- StateT (\st -> Right (show (typeCount st), incVarCount st))
    e1 <- typeInference t1
    e2 <- typeInference t2
    return (TypeExist [v1,v2] [TyProduct () (TyVar () v1) (TyVar () v2), e1, e2])
typeInference (FFix t) = undefined
typeInference (Pair t1 t2) = undefined
typeInference (PCase t1 ((v1,v2), t2)) = undefined
typeInference Unit = undefined
typeInference (UCase t1 t2) = undefined
typeInference Succ = undefined
typeInference Zero = undefined
typeInference (NCase t1 t2 (a, t3)) = undefined
typeInference Cons = undefined
typeInference Nil = undefined
typeInference (LCase t1 t2 (a, t3)) = undefined

