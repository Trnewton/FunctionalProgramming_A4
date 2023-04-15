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
    = TypeExist [TyTerm () var] [TypeEqns var]
    | TypeEqn (TyTerm () var, TyTerm () var)
    deriving (Show, Eq)

type LambdaTyEqns = TypeEqns String

typeEqPrettyShow :: LambdaTyEqns -> String
typeEqPrettyShow (TypeExist vs ts) = "(E|["
                    ++ concatMap (\v -> lambdaTyPrettyShow v ++ ",") vs
                    ++ "]."
                    ++ concatMap (\t -> typeEqPrettyShow t ++ ", ") ts
                    ++ ")"
typeEqPrettyShow (TypeEqn (ty1, ty2)) = "(" ++ lambdaTyPrettyShow ty1 ++ "=" ++ lambdaTyPrettyShow ty2 ++ ")"

type LambdaTypeEqns = TypeEqns String
type Context = [(LambdaTerm, LambdaTyTerm)]
type TypeVar = Int

data Stack = Data
    { context :: Context
    , typeCount :: Int }

incVarCount :: Stack -> Stack
incVarCount st = st { typeCount = typeCount st + 1}

newVar :: StateT Stack (Either String) LambdaTyTerm
newVar = StateT (\st -> Right (TyVar () (show (typeCount st)), st { typeCount = typeCount st + 1}))

getNewVar :: LambdaTerm -> StateT Stack (Either String) LambdaTyTerm
getNewVar t = do
    ty <- newVar
    modify (\st -> st { context = (t,ty):context st })
    return ty

lookUp :: LambdaTerm -> Context -> Maybe LambdaTyTerm
lookUp t [] = Nothing
lookUp t ((t', ty):ts)
    | t == t'   = Just ty
    | otherwise = lookUp t ts

lookUpVar :: LambdaTerm -> StateT Stack (Either String) LambdaTyTerm
lookUpVar t = StateT (\st -> case lookUp t (context st) of
                        (Just ty)   -> Right (ty,st)
                        Nothing     -> Left "Free Variable")

runTypeInference :: LambdaTerm -> Either String LambdaTypeEqns
runTypeInference t = evalStateT (typeInference (TyVar () "0") t) (Data { context=[], typeCount=1} )

typeInference :: LambdaTyTerm -> LambdaTerm -> StateT Stack (Either String) LambdaTypeEqns
typeInference tyQ (Var v) = do
    p <- lookUpVar (Var v)
    return (TypeEqn (p, tyQ))
typeInference tyQ (Abs x t) = do
    tyY <- newVar
    saveStack <- get
    tyX <- getNewVar (Var x)
    e <- typeInference tyY t
    modify (\st -> st { context = context saveStack })
    return (TypeExist [tyX, tyY] [TypeEqn (tyQ, TyArr () tyX tyY), e])
typeInference tyQ (App t1 t2) = do
    tyX <- newVar
    tyZ <- newVar
    e1 <- typeInference tyX t1
    e2 <- typeInference tyZ t2
    return (TypeExist [tyX,tyZ] [TypeEqn (tyZ, TyArr () tyX tyQ), e1, e2])
typeInference tyQ (FFix t) = do
    tyZ <- newVar
    e <- typeInference tyZ t
    return (TypeExist [tyZ] [TypeEqn (tyZ, TyArr () tyQ tyQ), e])
typeInference tyQ (Pair t1 t2) = do
    tyX <- newVar
    tyY <- newVar
    e1 <- typeInference tyX t1
    e2 <- typeInference tyY t2
    return (TypeExist [tyX,tyY] [TypeEqn (tyQ, TyProduct () tyX tyY), e1, e2])
typeInference tyQ (PCase t ((x,y), s)) = do
    tyZ <- newVar
    e1 <- typeInference tyZ t
    saveStack <- get
    tyX <- getNewVar (Var x)
    tyY <- getNewVar (Var y)
    e2 <- typeInference tyQ s
    modify (\st -> st { context = context saveStack })
    return (TypeExist [tyX,tyY,tyZ] [TypeEqn (tyZ, TyProduct () tyX tyY), e1, e2])
typeInference tyQ Unit = do
    return (TypeEqn (tyQ, TyUnit ()))
typeInference tyQ (UCase t s) = do
    tyZ <- newVar
    e1 <- typeInference tyZ t
    e2 <- typeInference tyQ s
    return (TypeExist [tyZ] [TypeEqn (tyZ, TyUnit ()), e1, e2])
typeInference tyQ Succ = do
    return (TypeEqn (tyQ, TyArr () (TyNat ()) (TyNat ())))
typeInference tyQ Zero = do
    return (TypeEqn (tyQ, TyNat ()))
typeInference tyQ (NCase t t0 (n, t1)) = do
    tyX1    <- newVar
    e1      <- typeInference tyX1 t
    tyY1    <- newVar
    e2      <- typeInference tyY1 t0
    tyY1    <- newVar
    tyY2    <- newVar
    saveStack <- get
    tyX2 <- getNewVar (Var n)
    e3      <- typeInference tyY2 t1
    modify (\st -> st { context = context saveStack })
    return (TypeExist [tyX1,tyY1,tyX2,tyY2] [
                TypeEqn (tyX1,TyNat ()),
                TypeEqn (tyX2,TyNat ()),
                TypeEqn (tyY1,tyQ),
                TypeEqn (tyY2,tyQ),
                e1,e2,e3 ])
typeInference tyQ Cons = do
    tyA <- newVar
    return (TypeExist [tyA] [TypeEqn (tyQ, TyProduct () (TyList () tyA) (TyList () tyA))])
typeInference tyQ Nil = do
    tyA <- newVar
    return (TypeExist [tyA] [TypeEqn (tyQ, TyList () tyA)])
typeInference tyQ (LCase t t0 (v, t1)) = do
    tyX    <- newVar
    tyX1    <- newVar
    e1      <- typeInference tyX1 t
    tyY1    <- newVar
    e2      <- typeInference tyY1 t0
    tyY1    <- newVar
    tyY2    <- newVar
    saveStack <- get
    tyX2 <- getNewVar (Var v)
    e3      <- typeInference tyY2 t1
    modify (\st -> st { context = context saveStack })
    return (TypeExist [tyX,tyX1,tyY1,tyX2,tyY2] [
                TypeEqn (tyX1,TyList () tyX),
                TypeEqn (tyX2,TyProduct () tyX (TyList () tyX)),
                TypeEqn (tyY1,tyQ),
                TypeEqn (tyY2,tyQ),
                e1,e2,e3 ])

