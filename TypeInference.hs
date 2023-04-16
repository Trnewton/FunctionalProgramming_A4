module TypeInference where

import Lib.Monads
import Lib.AST
import GHC.Base (undefined)

-- | autoGraderTypeInference
-- You should implement this function! This function should take a LambdaTerm
-- and output either:
--      - Left: an informative error message (either a free variable error,
--          or a type inferencing error such as match failure or occurs check)
--      - Right: the most general inferred type of the lambda term
-- Wahoo!! Last assignment you're nearly there :D
autoGraderTypeInference :: LambdaTerm -> Either String LambdaTyTerm
autoGraderTypeInference t = Right (snd (head sEqs))
    where
        eqs = case runGetTypeEqns t of
                Left msg    -> error msg
                Right eqs'   -> eqs
        (fv, bv, sEqs) = solveTypeEqns eqs



------------------------------------------------------------------------------------------
--  Data Structures
------------------------------------------------------------------------------------------

-------- General stuff --------

data TypeEqns var
    = TypeExist [TyTerm () var] [TypeEqns var]
    | TypeEqn (TyTerm () var, TyTerm () var)
    deriving (Show, Eq)
type LambdaTypeEqns = TypeEqns String

typeEqPrettyShow :: LambdaTypeEqns -> String
typeEqPrettyShow (TypeExist vs ts) = "(E|["
                    ++ concatMap (\v -> lambdaTyPrettyShow v ++ ",") vs
                    ++ "]."
                    ++ concatMap (\t -> typeEqPrettyShow t ++ ", ") ts
                    ++ ")"
typeEqPrettyShow (TypeEqn (ty1, ty2)) = "(" ++ lambdaTyPrettyShow ty1 ++ "=" ++ lambdaTyPrettyShow ty2 ++ ")"

-------- Stuff for getting type equations --------

type Context = [(LambdaTerm, LambdaTyTerm)]
data Stack = Data
    { context :: Context
    , typeCount :: Int }

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

-------- Stuff for solving type equations --------

type Substitution = (String, LambdaTyTerm)
type Assignment = (String, LambdaTyTerm)
type SolvedTyEqs var = ([var], [var], [(var, TyTerm () var)])
type LambdaSolvedTyEqs = SolvedTyEqs String

------------------------------------------------------------------------------------------
--  Get Type Equations
------------------------------------------------------------------------------------------

runGetTypeEqns :: LambdaTerm -> Either String LambdaTypeEqns
runGetTypeEqns t = evalStateT (getTypeEqns (TyVar () "0") t) (Data { context=[], typeCount=1} )

getTypeEqns :: LambdaTyTerm -> LambdaTerm -> StateT Stack (Either String) LambdaTypeEqns
getTypeEqns tyQ (Var v) = do
    p <- StateT (\st -> case lookUp (Var v) (context st) of
                        (Just ty)   -> Right (ty,st)
                        Nothing     -> Left "Free Variable")
    return (TypeEqn (p, tyQ))
getTypeEqns tyQ (Abs x t) = do
    tyY <- newVar
    saveStack <- get
    tyX <- getNewVar (Var x)
    e <- getTypeEqns tyY t
    modify (\st -> st { context = context saveStack })
    return (TypeExist [tyX, tyY] [TypeEqn (tyQ, TyArr () tyX tyY), e])
getTypeEqns tyQ (App t1 t2) = do
    tyX <- newVar
    tyZ <- newVar
    e1 <- getTypeEqns tyX t1
    e2 <- getTypeEqns tyZ t2
    return (TypeExist [tyX,tyZ] [TypeEqn (tyZ, TyArr () tyX tyQ), e1, e2])
getTypeEqns tyQ (FFix t) = do
    tyZ <- newVar
    e <- getTypeEqns tyZ t
    return (TypeExist [tyZ] [TypeEqn (tyZ, TyArr () tyQ tyQ), e])
getTypeEqns tyQ (Pair t1 t2) = do
    tyX <- newVar
    tyY <- newVar
    e1 <- getTypeEqns tyX t1
    e2 <- getTypeEqns tyY t2
    return (TypeExist [tyX,tyY] [TypeEqn (tyQ, TyProduct () tyX tyY), e1, e2])
getTypeEqns tyQ (PCase t ((x,y), s)) = do
    tyZ <- newVar
    e1 <- getTypeEqns tyZ t
    saveStack <- get
    tyX <- getNewVar (Var x)
    tyY <- getNewVar (Var y)
    e2 <- getTypeEqns tyQ s
    modify (\st -> st { context = context saveStack })
    return (TypeExist [tyX,tyY,tyZ] [TypeEqn (tyZ, TyProduct () tyX tyY), e1, e2])
getTypeEqns tyQ Unit = do
    return (TypeEqn (tyQ, TyUnit ()))
getTypeEqns tyQ (UCase t s) = do
    tyZ <- newVar
    e1 <- getTypeEqns tyZ t
    e2 <- getTypeEqns tyQ s
    return (TypeExist [tyZ] [TypeEqn (tyZ, TyUnit ()), e1, e2])
getTypeEqns tyQ Succ = do
    return (TypeEqn (tyQ, TyArr () (TyNat ()) (TyNat ())))
getTypeEqns tyQ Zero = do
    return (TypeEqn (tyQ, TyNat ()))
getTypeEqns tyQ (NCase t t0 (n, t1)) = do
    tyX1    <- newVar
    e1      <- getTypeEqns tyX1 t
    tyY1    <- newVar
    e2      <- getTypeEqns tyY1 t0
    tyY1    <- newVar
    tyY2    <- newVar
    saveStack <- get
    tyX2 <- getNewVar (Var n)
    e3      <- getTypeEqns tyY2 t1
    modify (\st -> st { context = context saveStack })
    return (TypeExist [tyX1,tyY1,tyX2,tyY2] [
                TypeEqn (tyX1,TyNat ()),
                TypeEqn (tyX2,TyNat ()),
                TypeEqn (tyY1,tyQ),
                TypeEqn (tyY2,tyQ),
                e1,e2,e3 ])
getTypeEqns tyQ Cons = do
    tyA <- newVar
    return (TypeExist [tyA] [TypeEqn (tyQ, TyProduct () (TyList () tyA) (TyList () tyA))])
getTypeEqns tyQ Nil = do
    tyA <- newVar
    return (TypeExist [tyA] [TypeEqn (tyQ, TyList () tyA)])
getTypeEqns tyQ (LCase t t0 (v, t1)) = do
    tyX    <- newVar
    tyX1    <- newVar
    e1      <- getTypeEqns tyX1 t
    tyY1    <- newVar
    e2      <- getTypeEqns tyY1 t0
    tyY1    <- newVar
    tyY2    <- newVar
    saveStack <- get
    tyX2 <- getNewVar (Var v)
    e3      <- getTypeEqns tyY2 t1
    modify (\st -> st { context = context saveStack })
    return (TypeExist [tyX,tyX1,tyY1,tyX2,tyY2] [
                TypeEqn (tyX1,TyList () tyX),
                TypeEqn (tyX2,TyProduct () tyX (TyList () tyX)),
                TypeEqn (tyY1,tyQ),
                TypeEqn (tyY2,tyQ),
                e1,e2,e3 ])


------------------------------------------------------------------------------------------
--  Unification
-----------------------------------------------------------------------------------------

-- |Unions to lists of variable names. Will maintain the order of lists and keep the
--  list unchanged.
union :: (Eq a) => [a] -> [a] -> [a]
union xs ys = xs ++ filter (`notElem` xs) ys

remove :: (Eq a) => a -> [a] -> [a]
remove element = filter (/= element)

match :: LambdaTyTerm -> LambdaTyTerm -> [Substitution]
match (TyVar () v) y = [(v, y)]
match x (TyVar () v) = [(v, x)]
match (TyArr () x1 x2) (TyArr () y1 y2) = match x1 y1 ++ match x2 y2
match (TyProduct () x1 x2) (TyProduct () y1 y2) = match x1 y1 ++ match x2 y2
match (TyNat ()) (TyNat ()) = []
match (TyUnit ()) (TyUnit ()) = []
match (TyList () x) (TyList () y) = match x y
match _ _ = error "Matching failed"

free :: LambdaTyTerm -> [String]
free (TyVar () v) = [v]
free (TyArr () x1 x2) = free x1 ++ free x2
free (TyProduct () x1 x2) = free x1 ++ free x2
free (TyNat ()) = []
free (TyUnit ()) = []
free (TyList () x) = free x

findAssign :: String -> [Assignment] -> Maybe Assignment
findAssign _ [] = Nothing
findAssign v ((w, t):rest)
    | v == w    = Just (w, t)
    | otherwise = findAssign v rest

appSubs :: Assignment -> [Assignment] -> [Assignment]
appSubs _ [] = []
appSubs (v, t) ((v',t'):rest)
    | v == v'   = (if t ==t' then [] else match t t') ++ appSubs (v, t) rest
    | otherwise = (v',subVar (v,t) t'):appSubs (v, t) rest

subVar :: Assignment -> LambdaTyTerm -> LambdaTyTerm
subVar (v, t) (TyVar () w)
    | v == w    = t
    | otherwise = TyVar () w
subVar (v, t) (TyArr () x1 x2) = TyArr () (subVar (v, t) x1)  (subVar (v, t) x2)
subVar (v, t) (TyProduct () x1 x2) = TyProduct () (subVar (v, t) x1) (subVar (v, t) x2)
subVar (v, t) (TyNat ()) = TyNat ()
subVar (v, t) (TyUnit ()) = TyNat ()
subVar (v, t) (TyList () x) = TyList () (subVar (v, t) x)

solveTypeEqns :: LambdaTypeEqns -> LambdaSolvedTyEqs
solveTypeEqns eqs = (fvOut,bvOut,subsOut)
    where
        (fv,bv,subs) = solveTypeEqns' eqs
        (fvOut,bvOut,subsOut) = (fv,bv,subs)

solveTypeEqns' :: LambdaTypeEqns -> LambdaSolvedTyEqs
solveTypeEqns' (TypeExist [] []) = ([], [], [])
solveTypeEqns' (TypeEqn (t1, t2)) = (free t1 ++ free t2, [], match t1 t2)
solveTypeEqns' (TypeExist nbv eqs) = process (foldr collect ([], nbv', []) eqs)
    where
        nbv' = map (\(TyVar () v) -> v) nbv

collect :: LambdaTypeEqns -> LambdaSolvedTyEqs -> LambdaSolvedTyEqs
collect eqs (fv, bv, subs) = (fvOut, bvOut, subsOut)
    where
        (fv', bv', subs') = solveTypeEqns' eqs
        -- TODO: This might be  wrong as per "disjoint union"
        fvOut = fv `union` fv'
        bvOut = bv `union` bv'
        subsOut = subs ++ subs'

-- TODO: Need to account for possible introduction of new assignment that allows for bv erradication
process :: LambdaSolvedTyEqs -> LambdaSolvedTyEqs
process (fv, [], subs) = (fv, [], subs)
process (fv, v:bv, subs) = case findAssign v subs of
    Nothing     -> let (fv', bv', subs') = process (remove v fv, bv, subs) in (fv', v:bv', subs')
    Just sub    -> let (fv', bv', subs') = process (remove v fv, bv, appSubs sub subs) in (fv', v:bv', subs')
