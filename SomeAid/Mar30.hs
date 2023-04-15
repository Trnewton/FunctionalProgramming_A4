-- unification

data Term
    = Op String [Term] -- operator/function
    | Var String 
    deriving (Show, Eq)

-- f(a,b) ==> Op "f" [Var "a", Var "b"]

-- what is a substitution?
-- intuitively, it is a mapping between Strings (variable names) to Terms (the term that you want to substitute when you see the corresponding variable name)

type Sub = [(String, Term)] -- [term / varname]
-- more formally, substitution is a partial function (that is, only 1 varaible maps to 1 term)
--

-- we want to apply a substitution to a term
applySub :: Sub -> Term -> Term
applySub sub term = f term
    where
    --
    f (Var var) = case lookup var sub of
                    Nothing -> Var var
                    Just t -> t
    f (Op opname args) = Op opname (map f args)


-- we can compose functions, so lets compose substitutions
-- f :: A -> B, g :: B -> C
-- g \circ f  (x) = g ( f ( x))
-- 
-- `comp` is \circ 
-- lets say you have
-- \tau \circ \sigma
comp :: Sub -> Sub -> Sub
comp tau sigma = 
    [ (v, applySub tau term) | (v, term) <- sigma ]
    ++
    [(v,t)  |  (v,t) <- tau, v `notElem` map fst sigma  ]
-- \sigma = [("a", Var "b")]
-- \tau =   [("a", Var "d")]
--  ......
-- \sigma = [("a", Var "b")]
-- \tau = [("b", Var "c")]
-- expect:
-- [
--  ("a", Var "c")
--  ,("b", Var "c")
-- ]
-- actual:
-- [
--  ("a", Var "c")
-- ]
sigma = [("a", Var "b")]
tau = [("b", Var "c")]

-- what is the unification problem?
-- given two terms S and T
-- we want to find a substitution \sigma s.t.
-- applySub \sigma S == applySub \sigma T
-- In this case, \sigma is called a \emph{unifier}
--
-- Examples:
-- Term
--  S = f(a)
--  T = g(a)
--  do we have a unifier? NOOO 
--  This is called a "match failure", since the names of the functions doesn't match
--
-- Term
--  S = f(a,b)
--  T = f(b,a)
--  do we have a unifier? YES
--  \sigma = [a/b] 
--  applySub \sigma S = f(a,a)
--  applySub \sigma T = f(a,a)
--
--Term
--  S = f(a)
--  T = b
-- [f(a)/b]
--
--Term
--  S = f(a)
--  T = a
-- we don't have a unifier. Why?
-- \sigma = [f(a)/a]
--  applySub \sigma S = f(f(a))
--  applySub \sigma T = f(a)
-- This is called the "occurs check", becaues a variable name of S "occurs" in T
--
-- Formally, a substitution \sigma is better than \tau if 
--  there exists a substitution \delta such that
--  \delta \circ \sigma = \tau
--
-- [(S_1, T_1), ..., (S_N, T_N)] (list of constraints)
-- VS
-- [(S_1,T_1)]

unify :: [(Term, Term)] -> Sub
unify [] = []
unify ((a,b): cs) | a == b = unify cs
unify (c: cs) = case c of
    -- we want to substitute "x" with y
    (Var x, y) | x `notElem` (freeVars y) -> 
                --you have a substitution (x,y)
                --we want to have:
                --  - first perform the substitution (x,y)
                --  - then perform the rest of the substitutions
                -- we apply the substitution (x,y) in cs for termination reasons:
                --  we want to get rid of all occurrences of the variable "x", which gives us 1 less varaible/constraint to unify
                unify [(applySub [(x,y)] l, applySub [(x,y)] r) | (l,r) <- cs]  `comp` [(x,y)]
        -- I.H. unify the tail gives us the most general unifier for the tail
        -- unify cs[y/x]
               | otherwise -> error "occurs check"
    -- symmetric case, just copy paste from above
    (y, Var x) -> undefined
    (Op f fargs, Op g gargs) | f == g && length fargs == length gargs
        -> unify (zip fargs gargs ++ cs)
                    |otherwise -> error "match failure"

freeVars :: Term -> [String]
freeVars (Var s) = [s]
freeVars (Op _ args) = concatMap freeVars args

