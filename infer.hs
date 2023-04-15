{-
{-# LANGUAGE TemplateHaskell, DeriveFoldable #-}

-- Exp, Pat, Dec Type Name

import Language.Haskell.TH
import Data.Functor.Foldable.TH
-- https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/derive-functor

cata :: [a] -> b


-- Q monad
f :: Q Exp
f = [|\x -> x + 1|]

data List a = Nil
  |Cons a (List a)
   deriving Foldable


data NatListF a = Ni | Con (Nat, a)
newtype Natlist = In (NatListF Natlist)

-- https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/derive-functor

-- recursion schemes
-- bananas, lenses, envolopes, barbed wire
-}
import Control.Monad.Reader
import Control.Monad.State

data Term = Var String
  | Op String [Term]
  deriving Eq

type Sub = [(String, Term)]

applySub :: Sub -> Term -> Term
applySub s t = case t of
                 Op f ts -> Op f $ map (applySub s) ts
                 Var n -> case lookup n s of
                   Nothing -> Var n
                   Just t' -> t'



comp :: Sub -> Sub -> Sub
comp f g = [(v, applySub f t) | (v, t) <- g]
  ++ filter p f
  where p (x, y) = notElem x g'
        g' = map fst g


-- g == ["a", Var "y"]
-- f == [("b", Var "x"), ("y", Var "x")]

-- S = a
-- T = g(b)

-- [(a, g(f(c))), (b, f(c))]
-- [(a, g(c)), (b, c)]

-- g(X, Y, Z)
-- g(A, B, C)
-- [(X,A),(Y,B),(Z,C)]

unify :: [(Term, Term)] -> Maybe Sub
unify [] = Just []
unify ((x,y):xs) | x == y = unify xs
unify (c:xs) = case c of
                 (Var x, y) | notElem x $ freeVars y ->
                              case unify $ map (apair (applySub [(x,y)])) xs of
                                Just u -> Just $ comp u [(x, y)]
                                _ -> Nothing
--               (x, Var y) | try this one out, it's symmetric to the first case
                 (Op f fs, Op g gs) | f == g && length fs == length gs ->
                                        unify (zip fs gs ++ xs)
                 _ -> Nothing
  where
    freeVars :: Term -> [String]
    freeVars (Var v) = [v]
    freeVars (Op _ t) = t >>= freeVars
    
    apair :: (a -> b) -> (a, a) -> (b, b)
    apair f (x, y) = (f x, f y)

--------------------
data Lambda =
  Var String
  | App Lambda Lambda
  | Abs String Lambda
  deriving Show

type Type a = StateT Int (Reader (Int, [(String, Int)])) a

typeinfer :: Lambda -> Term
typeinfer l = applySub sub (Var $ "0")
  where
    consts = runReader (evalStateT (gentypes l) 1) (0, [])
    sub = unify consts
    

ask :: Reader s m
ask = Reader \s -> s

gentypes :: Lambda -> Type [(Term, Term)]
gentypes (Var s) = do
  (ty, env) <- ask
  case lookup s env of
    Noting -> error "free var"
    Just vty -> return $ [(Var $ show ty, Var $ show vty)]
gentypes (App l r) = do
  (ty, env) <- ask
  lty <- state (\new -> (new, new + 1))
  rty <- state (\new -> (new, new + 1))
  -- :t state ===>
  -- state :: MonadState s m => (s -> (a, s)) -> m a
  lconst <- local (\(_, e) -> (lty, e)) $ gentypes l
  rconst <- local (\(_, e) -> (rty, e)) $ gentypes r
  -- local :: (s -> s) -> Reader s a -> Reader s a
  -- local f (Reader g) = Reader $ \s -> g (f s)
  return $
[(Var $ show lty, Op "->" [Var $ show rty, Var $ show ty
                          ]
 )] ++ lconst ++ rconst
gentypes (Abs s t) = do
  (ty, env) <- ask
  xty <- state (\new -> (new, new + 1))
  tty <- state (\new -> (new, new + 1))
  const <- local (\(_, e) -> (tty, (s, xty):e))
           $ gentypes t
  return $ (Var $ show ty,
            Op "->" [Var $ show xty, Var $ show tty]
           ):const




