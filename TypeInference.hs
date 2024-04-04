module TypeInference where

import Lib.Monads
import Lib.AST
import Control.Monad (foldM)

-- | autoGraderTypeInference
-- You should implement this function! This function should take a LambdaTerm
-- and output either:
--      - Left: an informative error message (either a free variable error,
--          or a type inferencing error such as match failure or occurs check)
--      - Right: the most general inferred type of the lambda term
-- Wahoo!! Last assignment you're nearly there :D
autoGraderTypeInference :: LambdaTerm -> Either String LambdaTyTerm
autoGraderTypeInference t = do
    eqs <- runGetTypeEqns t
    solveTypeEqns eqs

------------------------------------------------------------------------------------------
--  Data Structures
------------------------------------------------------------------------------------------

-------- General stuff --------

-- | Type equation
data TypeEqns var
    -- | Existential quantifier
    = TypeExist [var] [TypeEqns var]
    -- | Type equals
    | TypeEqn (TyTerm () var, TyTerm () var)
    deriving (Show, Eq)
type LambdaTypeEqns = TypeEqns String

-- | Pretty printing for type equations
typeEqPrettyShow :: LambdaTypeEqns -> String
typeEqPrettyShow (TypeExist vs ts) = "(["
                    ++ concatMap (++ ",") vs
                    ++ "]."
                    ++ concatMap (\t -> typeEqPrettyShow t ++ ",") ts
                    ++ ")"
typeEqPrettyShow (TypeEqn (ty1, ty2)) = "(" ++ lambdaTyPrettyShow ty1 ++ "=" ++ lambdaTyPrettyShow ty2 ++ ")"

-------- Stuff for getting type equations --------

-- | Type context data structure
type Context = [(String, LambdaTyTerm)]
data Stack = Data {
    -- | Type context
    context :: Context,
    -- | Counter for giving new type variables unique names
    typeCount :: Int }

-- | Creates a new type variable
newVar :: StateT Stack (Either String) (String, LambdaTyTerm)
newVar = StateT newVar'
    where
    newVar' :: Stack -> Either String ((String, LambdaTyTerm), Stack)
    newVar' st = Right ((v, tyV), st { typeCount = typeCount st + 1})
        where
            v = show (typeCount st)
            tyV = TyVar () v

-- | Creates a new type variable and adds it to the context
getNewVar :: String -> StateT Stack (Either String) (String, LambdaTyTerm)
getNewVar t = do
    (v, tyV) <- newVar
    modify (\st -> st { context = (t,tyV):context st })
    return (v, tyV)

-- | Looks up a variable on the context
lookUp :: String -> Context -> Maybe LambdaTyTerm
lookUp t [] = Nothing
lookUp t ((t', ty):ts)
    | t == t'   = Just ty
    | otherwise = lookUp t ts

-------- Stuff for solving type equations --------

type Substitution = (String, LambdaTyTerm)
type Assignment = (String, LambdaTyTerm)
-- | Type for solvig type equations (free variables, bound variables, substitions)
type SolvedTyEqs var = ([var], [var], [(var, TyTerm () var)])
type LambdaSolvedTyEqs = SolvedTyEqs String

-- | Pretty printing for solving type equations
solTyEqPrettyShow :: LambdaSolvedTyEqs -> String
solTyEqPrettyShow (_,_,eqs) = "[" ++ concatMap (\(v,ty) -> v ++ "=" ++ lambdaTyPrettyShow ty ++ ",") eqs ++ "]"

------------------------------------------------------------------------------------------
--  Get Type Equations
------------------------------------------------------------------------------------------

-- | Gets type equations for lambda term
runGetTypeEqns :: LambdaTerm -> Either String LambdaTypeEqns
runGetTypeEqns t = evalStateT (getTypeEqns (TyVar () "0") t) (Data { context=[], typeCount=1} )

-- | State monad for getting type equations for lambda term
getTypeEqns :: LambdaTyTerm -> LambdaTerm -> StateT Stack (Either String) LambdaTypeEqns
-- proj
getTypeEqns tyQ (Var v) = do
    p <- StateT (\st -> case lookUp v (context st) of
                        (Just ty)   -> Right (ty,st)
                        Nothing     -> Left "Free Variable")
    return (TypeEqn (p, tyQ))
-- asbt
getTypeEqns tyQ (Abs x t) = do
    (y, tyY)<- newVar
    saveST  <- get
    (x',tyX)<- getNewVar x
    e       <- getTypeEqns tyY t
    modify (\st -> st { context = context saveST })
    return (TypeExist [x',y] [TypeEqn (tyQ, TyArr () tyX tyY), e])
-- app
getTypeEqns tyQ (App t1 t2) = do
    (x,tyX) <- newVar
    (y,tyZ) <- newVar
    e1      <- getTypeEqns tyX t1
    e2      <- getTypeEqns tyZ t2
    return (TypeExist [x,y] [TypeEqn (tyX, TyArr () tyZ tyQ), e1, e2])
-- fix
getTypeEqns tyQ (FFix t) = do
    (z,tyZ) <- newVar
    e       <- getTypeEqns tyZ t
    return (TypeExist [z] [TypeEqn (tyZ, TyArr () tyQ tyQ), e])
-- pair
getTypeEqns tyQ (Pair t1 t2) = do
    (x,tyX) <- newVar
    (y,tyY) <- newVar
    e1      <- getTypeEqns tyX t1
    e2      <- getTypeEqns tyY t2
    return (TypeExist [x,y] [TypeEqn (tyQ, TyProduct () tyX tyY), e1, e2])
-- pcase
getTypeEqns tyQ (PCase t ((x,y), s)) = do
    (z,tyZ) <- newVar
    e1      <- getTypeEqns tyZ t
    saveST  <- get
    (x',tyX)<- getNewVar x
    (y',tyY)<- getNewVar y
    e2      <- getTypeEqns tyQ s
    modify (\st -> st { context = context saveST })
    return (TypeExist [x',y',z] [TypeEqn (tyZ, TyProduct () tyX tyY), e1, e2])
-- unit
getTypeEqns tyQ Unit = do
    return (TypeEqn (tyQ, TyUnit ()))
-- ucase
getTypeEqns tyQ (UCase t s) = do
    (z,tyZ) <- newVar
    e1      <- getTypeEqns tyZ t
    e2      <- getTypeEqns tyQ s
    return (TypeExist [z] [TypeEqn (tyZ, TyUnit ()), e1, e2])
-- succ
getTypeEqns tyQ Succ = do
    return (TypeEqn (tyQ, TyArr () (TyNat ()) (TyNat ())))
-- zero
getTypeEqns tyQ Zero = do
    return (TypeEqn (tyQ, TyNat ()))
-- N case
getTypeEqns tyQ (NCase t t0 (n, t1)) = do
    (x1,tyX1)   <- newVar
    e1          <- getTypeEqns tyX1 t
    (y1,tyY1)   <- newVar
    e2          <- getTypeEqns tyY1 t0
    (y2,tyY2)   <- newVar
    saveST      <- get
    (x2,tyX2)   <- getNewVar n
    e3          <- getTypeEqns tyY2 t1
    modify (\st -> st { context = context saveST })
    return (TypeExist [x1,y1,x2,y2] [
                TypeEqn (tyX1,TyNat ()),
                TypeEqn (tyX2,TyNat ()),
                TypeEqn (tyY1,tyQ),
                TypeEqn (tyY2,tyQ),
                e1,e2,e3 ])
-- cons
getTypeEqns tyQ Cons = do
    (a,tyA) <- newVar
    return (TypeExist [a] [TypeEqn (tyQ,
        TyArr ()
        (TyProduct ()
            tyA
            (TyList () tyA))
        (TyList () tyA)
        )])
-- nil
getTypeEqns tyQ Nil = do
    (a,tyA) <- newVar
    return (TypeExist [a] [TypeEqn (tyQ, TyList () tyA)])
-- L case
getTypeEqns tyQ (LCase t t0 (v, t1)) = do
    (x,tyX)     <- newVar
    (x1,tyX1)   <- newVar
    e1          <- getTypeEqns tyX1 t
    (y1,tyY1)   <- newVar
    e2          <- getTypeEqns tyY1 t0
    (y2,tyY2)   <- newVar
    saveST      <- get
    (x2,tyX2)   <- getNewVar v
    e3          <- getTypeEqns tyY2 t1
    modify (\st -> st { context = context saveST })
    return (TypeExist [x,x1,y1,x2,y2] [
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

-- | Takes the set difference of first list minus the second.
diff :: (Eq a) => [a] -> [a] -> [a]
diff xs ys = filter (`notElem` ys) xs

-- | Solves type equations and returns lambda type
solveTypeEqns :: LambdaTypeEqns -> Either String LambdaTyTerm
solveTypeEqns eqs = do
    (fv,bvOut,subs) <- solveTypeEqns' eqs
    (_,subsOut) <- linearize fv subs
    if null subsOut
        then
            return (TyVar () (head fv))
        else
            (case findAssign (head fv) subsOut of
                        Nothing -> return (TyVar () (head fv))
                        Just (_,t) -> return t)

-- | Solves type equations and returns (free variables, bound variables, substitutions)
solveTypeEqns' :: LambdaTypeEqns -> Either String LambdaSolvedTyEqs
solveTypeEqns' (TypeExist [] []) = Right ([], [], [])
solveTypeEqns' (TypeEqn (t1,t2)) = do
    subs <- match t1 t2
    return (free t1 ++ free t2, [], subs)
solveTypeEqns' (TypeExist nbv eqs) = do
    -- Apply algo recursively to sub-equations
    (fv, bv, subs) <- foldM collect ([], nbv, []) eqs
    -- Remove newly bound variables from free
    fvOut <- Right (diff fv bv)
    -- Do subsitutions that we can
    (bvOut, subsOut) <- linearize bv subs
    return (fvOut, bvOut, subsOut)

-- | Performs matching on type terms
match :: LambdaTyTerm -> LambdaTyTerm -> Either String [Substitution]
match (TyVar () v) y = Right [(v,y)]
match x (TyVar () v) = Right [(v,x)]
match (TyArr () x1 x2) (TyArr () y1 y2) = do
    l <- match x1 y1
    r <- match x2 y2
    return (l ++ r)
match (TyProduct () x1 x2) (TyProduct () y1 y2) = do
    l <- match x1 y1
    r <- match x2 y2
    return (l ++ r)
match (TyNat ()) (TyNat ()) = Right []
match (TyUnit ()) (TyUnit ()) = Right []
match (TyList () x) (TyList () y) = match x y
match x y = Left ("Matching failed: " ++ lambdaTyPrettyShow x ++ ", " ++ lambdaTyPrettyShow y)

-- | Collects free variables in lambda term
free :: LambdaTyTerm -> [String]
free (TyVar () v) = [v]
free (TyArr () x1 x2) = free x1 ++ free x2
free (TyProduct () x1 x2) = free x1 ++ free x2
free (TyNat ()) = []
free (TyUnit ()) = []
free (TyList () x) = free x

-- | Checks for a substitution of given variable
findAssign :: String -> [Assignment] -> Maybe Assignment
findAssign _ [] = Nothing
findAssign v ((w, t):rest)
    | v == w    = Just (w, t)
    | otherwise = findAssign v rest

-- | Checks if variables occurs in lambda term
occursIn :: String -> LambdaTyTerm -> Bool
occursIn s (TyVar () v)
    | s == v    = True
    | otherwise = False
occursIn s (TyArr () x1 x2) = occursIn s x1 || occursIn s x2
occursIn s (TyProduct () x1 x2) = occursIn s x1 || occursIn s x2
occursIn s (TyNat ()) = False
occursIn s (TyUnit ()) = False
occursIn s (TyList () x) = occursIn s x

-- | Performs occurs check
occursCheck :: String -> LambdaTyTerm -> Either String ()
occursCheck s t = if occursIn s t then Left ("Occurs check fails: " ++ s ++ " in " ++ show t) else Right ()

-- | Applies a subsitution to a list of substitutions
appSubs :: Assignment -> [Assignment] -> Either String [Assignment]
appSubs _ [] = Right []
appSubs (v, t) ((v',t'):rest)
    | v/=v' = do -- Apply substitution
        rest' <- appSubs (v,t) rest
        subNew <- Right (v',subVar (v,t) t')
        return (subNew:rest')
    | t==t' = do -- Remove term
        rest' <- appSubs (v,t) rest
        return ((v',t'):rest')
    | otherwise = do -- Perform matching
        b <- match t t'
        c <- appSubs (v,t) rest
        return (b ++ c)

-- | Applies substitution to lambda term
subVar :: Assignment -> LambdaTyTerm -> LambdaTyTerm
subVar (v,t) (TyVar () w)
    | v == w    = t
    | otherwise = TyVar () w
subVar (v,t) (TyArr () x1 x2) = TyArr () (subVar (v,t) x1)  (subVar (v,t) x2)
subVar (v,t) (TyProduct () x1 x2) = TyProduct () (subVar (v,t) x1) (subVar (v,t) x2)
subVar (v,t) (TyNat ()) = TyNat ()
subVar (v,t) (TyUnit ()) = TyUnit ()
subVar (v,t) (TyList () x) = TyList () (subVar (v,t) x)

-- | Applies all possible subsitutions from list to list of variables
linearize :: [String] -> [Substitution] -> Either String ([String], [Substitution])
linearize bv subs = do
    (bv',subs',flag) <- linearize' bv subs
    if flag
        then linearize bv' subs'
        else return (bv',subs')

-- | Applies all possible subsitutions from list to list of variables
linearize' :: [String] -> [Substitution] -> Either String ([String], [Substitution], Bool)
linearize' [] subs = Right ([], subs, False)
linearize' (v:bv) subs = do
    case findAssign v subs of
        Nothing -> do -- No subs, move to next bound variable
            (bv', subs', flag) <- linearize' bv subs
            return (v:bv', subs', flag)
        Just sub -> do -- Apply subs and do occurs check
            occursCheck v $ snd sub
            subs' <- appSubs sub subs
            (bv', subs'', _) <- linearize' bv subs'
            return (bv', subs'', True)

-- | Collects result of solving type equations from existential type equations
collect :: LambdaSolvedTyEqs -> LambdaTypeEqns -> Either String LambdaSolvedTyEqs
collect (fv, bv, subs) eqs  = do
    (fv', bv', subs') <- solveTypeEqns' eqs
    fvOut <- Right (fv `union` fv')
    bvOut <- Right (bv `union` bv')
    subsOut <- Right (subs ++ subs')
    return (fvOut, bvOut, subsOut)
