-- as for final review, let's do matching step in unification
-- Q1:
data Exp v = Var v | Opn String [Exp v]
    deriving Show

data SF a = SS a | FF 
    deriving Show

match :: Eq v => Exp v -> Exp v -> SF [(v,Exp v)]
match ( Var x) t = check x t
match t (Var x)  = check x t
match (Opn f1 t1) (Opn f2 t2) | f1 == f2 && length t1 == length t2
    --zip the arguments, recurse ... but need to take care of the FF case
    --so we fold it over
    =  foldr f (SS []) $ zip t1 t2
    where
    f _ FF = FF
    f (a, b) (SS acc) = case match a b of
                          SS list -> SS $ list ++ acc
                          FF -> FF
match _ _ = FF

-- check for occurs check, if successful, return the singleton sub
-- we will also check for the trivial case here ...
-- what's the trivial case? is when two terms are equal ...
check :: Eq v => v -> Exp v  -> SF [(v,Exp v)]
check x (Var t) | x  == t = SS []
check x t | occursCheck x t = FF
          | otherwise = SS $ [(x,t)]

occursCheck :: Eq v => v -> Exp v  -> Bool
occursCheck v exp =  elem v (collect exp)

collect :: Exp v -> [v]
collect (Var v) = [v]
collect (Opn _  list) = concat (map collect list)

-- should occurs check
matchExample2 =
    match
        (Var 'a')
        (Opn "s1" [Var 'a'])

-- f(g(x), y) =?= f(y,z)
-- [g(x)/y, z/y]
matchExample5 = 
    match 
         (Opn "f" [Opn "g" [Var "x"], Var "y"]) 
         (Opn "f" [Var "y", Var "z"])

-- f(g(x,y),z) =?= f(g(h(v), w), g(v,w))
-- [h(v)/x, w/y, g(v,w)/z]
matchExample6 =
    match
        (Opn "f" [Opn "g" [Var 'x', Var 'y'], Var 'z'])
        (Opn "f" [Opn "g" [Opn "h" [Var 'v'], Var 'w'], Opn "g" [Var 'v', Var 'w']])
-- f(g(x,z),z) =?= f(g(w,w),g(h(v),x))
-- [w/x, w/z, g(h(v), x)/z]
matchExample7 =
    match
        (Opn "f" [Opn "g" [Var 'x', Var 'z'], Var 'z'])
        (Opn "f" [Opn "g" [Var 'w', Var 'w'], Opn "g" [Opn "h" [Var 'v'], Var 'x']])


{-
- Computability results you should review:
    - standardization theorem (normal order reduction gets normal)
    - second recursion theorem and implement the function $T$
    - scott curry theorem 
    -
- represent datatype in lambda  calculus (done from midterm)
-
- trace CES machine
- know type deduction (the in class way ... )

-}

