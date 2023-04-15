
----------------------
-- the monad stuff are copied from Monad.hs in your assignment ...
----------------------

----------------------
-- MonadTrans
----------------------
-- This is the type class to lift a monad to another monad..
-- lift is used to take a monad and bring the monad up to another monad
-- transformer.
class MonadTrans t where
    lift :: Monad m => m a -> t m a

----------------------
-- Identity monad
-- Generally speaking, this monad is useful when used with
-- monad transformers (e.g., StateT) when we just want to
-- use the monad WITHOUT combining another monad.. See
-- StateT for an example
----------------------
data Identity a = Identity a 

-- | runIdentity
-- This will take a value outside of the identity..
runIdentity :: Identity a -> a
runIdentity (Identity a) =  a

instance Functor Identity where
    fmap f (Identity n) = Identity $ f n

instance Applicative Identity where
    pure = Identity
    Identity f <*> n = fmap f n

instance Monad Identity where
    return = pure
    Identity n >>= f = f n

----------------------
-- StateT monad
-- This is the State monad, BUT it lets you combine it with
-- the effects of any OTHER monad... This is useful to combine
-- with the Either monad provided in the Haskell standard libraries
-- (Prelude!)
----------------------
-- | This is the StateT type..
-- Here is an explanation of the type variables:
--      - s: is the state of the computation
--      - m: is the monad for which this is combined with
--      - a: is the generic output type of the function
-- An explanation of (s -> m (a, s)). This type is saying that
-- if you give it a state, it will return (a, s) in the monad m i.e.,
-- it will give you a value ``a" and a new state ``s" all in the monad m.
data StateT s m a = StateT (s -> m (a, s))

-- | runStateT
-- Given a (StateT s m a), and a state s, this will run
-- the (StateT s m a) with the given state s, and output
-- m (s, a) where m is the inner monad.
runStateT :: Monad m => StateT s m a -> s -> m (a,s)
runStateT (StateT f) s = f s 

-- | evalStateT
-- Similar to runStateT, but only returns the output value
evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT st = fmap fst . runStateT st

-- | execStateT
-- Similar to runStateT, but only returns the output state
execStateT :: Monad m => StateT s m a -> s -> m s
execStateT st = fmap snd . runStateT st

-- | put 
-- Given a new state ``s", this function will
-- set the state to be ``s" for all computations
-- after this call.
-- In other words, it will put the new state in.
put :: Monad m => s -> StateT s m ()
put s' = StateT (\s -> return ((), s'))
-- Alternative implementation:
-- put s = modify (const s)

set :: Monad m => s -> StateT s m ()
set = put

-- | get
-- This will return the current state
get :: Monad m => StateT s m s
get = StateT (\s -> return (s , s))
-- Alternative implementation
-- get = gets id

-- | gets
-- Given a function f, this will return the current
-- state AFTER applying f to the state
gets :: Monad m => (s -> s') -> StateT s m s'
gets f = StateT (\s -> return (f s, s))

-- | modify
-- Given a function ``f :: s -> s", this will
-- apply ``f" to the current state, and modify 
-- the state for the computations after this call.
modify :: Monad m => (s -> s) -> StateT s m ()
modify f = StateT (\s -> return ((), f s))

-- | This is a type alias for the StateT monad
-- to give us the original state monad that 
-- is NOT combined with any other monad.
type State s a = StateT s Identity a

-- | runState
-- Similar to runStateT
runState :: State s a -> s -> (a,s)
runState st = runIdentity . runStateT st

-- | evalState
-- Similar to evalStateT
evalState :: State s a -> s -> a
evalState st = runIdentity . evalStateT st

-- | execState
-- Similar to execStateT
execState :: State s a -> s -> s
execState st = runIdentity . execStateT st

-- Functor instance (don't worry about this too much)
-- didn't go through this in class
instance Monad m => Functor (StateT s m) where
    fmap f (StateT g) = StateT $ \s -> do
        (a, s') <- g s
        return (f a, s')

-- Applicative instance (don't worry about this too much)
-- didn't go through this in class
instance Monad m => Applicative (StateT s m) where
    pure a = StateT $ \s -> pure (a,s) 

    StateT f <*> StateT a = StateT $ \s -> do
        (f', s') <- f s
        (a', s'') <- a s' 
        pure (f' a', s'')


instance Monad m => Monad (StateT s m) where
    -- return :: a -> (StateT s m) a
    return = pure
    -- (>>=) :: (StateT s m) a -> (a -> (StateT s m) b) -> (StateT s m) b
    StateT f >>= g = StateT (\s -> do
            (a, s')  <- f s 
            let StateT f' = g a 
            (b, s'') <- f' s'
            return (b, s'')
        )

-- This instance tells us how to take ANY monad and LIFT it up to the
-- StateT monad.. In other words, this tell us how to combine any monad
-- with the StateT monad..
instance MonadTrans (StateT s) where 
    -- lift :: m a -> StateT m a
    lift ma = StateT $ \s -> do
            a <- ma
            return (a, s)

--------------------------------------------
-- ReaderT monad (not discussed in tutorial)
-- This is the same as the State monad, but
-- the environment cannot be modified.
--------------------------------------------
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

type Reader r a = ReaderT r Identity a

runReader :: Reader r a -> r -> a
runReader env = runIdentity . runReaderT env

instance Monad m => Functor (ReaderT r m) where
    fmap f (ReaderT g) = ReaderT (fmap f . g)

instance Monad m => Applicative (ReaderT r m) where
    pure n = ReaderT $ \r -> pure n

    f <*> n = ReaderT $ \r -> do
        f' <- runReaderT f r
        n' <- runReaderT n r
        pure $ f' n'
                                
instance Monad m => Monad (ReaderT r m) where
    return = pure

    n >>= f = ReaderT $ \r -> do 
        n' <- runReaderT n r
        runReaderT (f n') r

instance MonadTrans (ReaderT r) where
    -- lift :: Monad m => m a -> ReaderT r m a
    -- lift = ReaderT . const
    lift m = ReaderT $ \_ -> m

asks :: Monad m => 
    (r -> r') -> 
    ReaderT r m r'
asks f = ReaderT (\r -> pure (f r))

ask :: Monad m => ReaderT r m r
ask = asks Prelude.id

local :: 
    -- to modify local env
    (r -> r) -> 
    -- to run with modified env
    ReaderT r m a ->
    -- regular env..
    ReaderT r m a 
local f (ReaderT g) = ReaderT $
    \r -> g (f r)

-------------------------------------------------
-- the unification is copied from last week's tutorial
-------------------------------------------------
data Term
    = Op String [Term]
    | Var String
    deriving (Show, Eq)

-- eg:
-- f(a,b) is Op "f" [Var "a", Var "b"]
-- we will apply functions to arguments ...

-- what is a substitution
-- a substitution is a mapping from String to Term
-- one way to represent this is
type Sub = [(String, Term)]
-- it is a function as a cartesian product
-- [("a", Var "b"), ("b", Op "f" [])]
-- then can use the lookup function to get the corresponding term corresponding to a variable

-- why we want these partial functions? 
-- we would like to apply these to terms: we want to replace ALL variables in a term (that corresponds to a substitution) with its corresponding term (just beta reduction substitution)
applySub:: Sub -> Term -> Term
applySub sub term = f term
    where
    f (Var v) = case lookup v sub of
                    Nothing -> Var v
                    Just t' -> t'
    f (Op fname args) = Op fname (map f args)

-- does the order of substitution matter? Yes, but the 1st projection of tuple SHOULD be unique, so its fine

-- we can compose substitutions
-- give f, g, gives g \circ f
-- tau `comp` sigma
comp:: Sub -> Sub -> Sub
comp tau sigma = 
    [(v, applySub tau t) | (v,t) <- sigma]
    ++
    [(v,t) | (v,t) <- tau, v `notElem` map fst sigma]
    
--       list of constraints
unify :: [(Term, Term)] -> Sub
unify [] = []
unify ((a,b):cs) | a == b = unify cs --trivial case: identical terms, no need substitute
unify (c:cs) = case c of
    (Var x, y) | x `notElem` freeVars y -> 
            --why do we substitute cs before recurse?
            --we also want to eliminate x in further substitutions (needed for termination)
            --you are decreasing the nvars
            --hence you "eliminate" all occurences of the variable "x" in the recursive call
             unify [ (applySub [(x,y)] l, applySub [(x,y)] r)| (l,r) <- cs] `comp` [(x,y)]
            -- ^^ is a unifier, we applied (x,y) in it
            --    it is the general unifier with cs[x/y]
            -- so we need to compose with [x/y] to include the unfier for (Var x,y) at the beginning
            --
            -- NOTE: if you don't put guard here, it will go down in the pattern matching ...
            | otherwise -> error "occurs check"

    --symmetric
    (x', Var y') | y' `notElem` freeVars x' -> 
            let x = y'
                y = x'
            in unify [ (applySub [(x,y)] l, applySub [(x,y)] r)| (l,r) <- cs] `comp` [(x,y)]
                | otherwise -> error "occurs check"

    (Op f fargs, Op g gargs) | f == g && length fargs == length gargs ->
        -- get rid of f = g, so -2 here
            unify (zip fargs gargs ++ cs)
                             | otherwise -> error "match failure"
            --if have 2 operators, they should have the same name and same number of arguments
    -- _ -> error "no unifiers found"


-- all variables in a term
freeVars :: Term -> [String]
freeVars (Var v)= [v]
freeVars (Op _ args) = concatMap freeVars args


-- given a lambda term, I want to return the type of the term
data Lam
    = LVar String
    | LApp Lam Lam
    | LAbs String Lam

-- in your assignment, "Term" would be the type terms
--data Term
--    = Op String [Term]
--    | Var String
--    deriving (Show, Eq)


type Uniq = Int
-- put reader inside state doesn't really work (without GHC extensions ... )
-- type TypeInfer a = StateT Uniq (Reader (Uniq, [(String, Uniq)])) a

-- for the stack and the recursion, we need to input 
-- (Uniq, [(String, Uniq)])
-- ^^         ^^ stack to look up the variables
-- current typeid of tree
--
-- newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
-- data StateT s m a = StateT (s -> m (a, s))
type TypeInfer a = ReaderT (Uniq, [(String, Uniq)]) (StateT Uniq (Either String)) a

state :: Monad m => (s -> m (a,s)) -> StateT s m a
state f = StateT f

-- idea: when entering the recursion, use "ask" to get the current type id of the tree 
-- then use state monad to get fresh var id (need to lift)
-- then use local to recurse with a different type id and a potentially different stack

runTypeInfer :: Lam -> Either String Term
runTypeInfer lam = 
    do -- in either monad
        -- you can say: the root typeid is "0", and of course we start with the empty stack
        -- then, since the root is type "0", you probably want to start the fresh id at "1"
        constraints <- evalStateT (runReaderT (typeInfer lam) (0, [])) 1
        -- constraints is of type [(Term, Term)]
        -- get the substitutions from unification
        let subs = unify constraints --subs is [(String, Term)]
        -- lastly, apply the substitutions to the root type, which is "0"
        return $ applySub subs (Var $ show 0)

--collect the constraints for unification
typeInfer :: Lam -> TypeInfer [(Term, Term)]
typeInfer (LVar v) = 
    do
      (mytype, env) <- ask
      case lookup v env of
            Just vartype -> return $ [(Var $ show mytype, Var $ show vartype)]
            Nothing ->  lift $ lift $ Left $ "free variable"

-- (l) (r) :: \alpha  (mytype)
-- l :: \beta  (ltype)
-- r :: \gamma (rtype)
--
-- \beta === \gamma -> \alpha
typeInfer (LApp l r) =
    do -- this "do" is inside the reader monad
      (mytype, _env) <- ask

      ltype <- lift $ state (\freshid -> Right $ (freshid, freshid+1))
      rtype <- lift $ state (\freshid -> Right $ (freshid, freshid+1))

      -- when recursing to the left subtree, set the expected typeid to "ltype", with the same environment
      lconstraints <- local (\(_, env) -> (ltype, env)) $ typeInfer l
      rconstraints <- local (\(_, env) -> (rtype, env)) $ typeInfer r

      return $ [(Var $ show ltype,  
                Op "->" [ Var $ show rtype, Var $ show mytype])] 
             ++ lconstraints ++ rconstraints
-- (\x . body) :: \alpha
-- x :: \beta
-- body :: \gamma
--
-- \alpha == \beta -> \gamma
typeInfer (LAbs x body) =
    do -- this "do" is inside the reader monad
      (mytype, _env) <- ask

      xtype <- lift $ state (\freshid -> Right $ (freshid, freshid+1))
      bodytype <- lift $ state (\freshid -> Right $ (freshid, freshid+1))

      -- when recurisng to the body, set the expected typeid to "bodytype", 
      -- also we need to push the type of "x" to the stack
      -- so do "(x, xtype) :env"
      bodyconstraints <- local (\(_, env) -> (bodytype, (x, xtype) :env)) $ typeInfer body

      return $ [(Var $ show mytype,
                Op "->" [ Var $ show xtype, Var $ show bodytype])] 
             ++ bodyconstraints

--examples?
--(\x.x)
eg0 = runTypeInfer $ (LAbs "x" (LVar "x"))

--(\x.x) (\xy.y)
eg1 = runTypeInfer$  ((LAbs "x" (LVar "x")) `LApp` ( LAbs "x" ( LAbs "y" (LVar "y"))))

-- can't type this since it's a free variable ...
eg2 =  runTypeInfer$  ((LVar "x"))

--(\x.x) (\x.x)
eg3 = runTypeInfer$  ((LAbs "x" (LVar "x")) `LApp` ( LAbs "x" (LVar "x")))






