{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Web.Zwaluw (
    -- * Types
    Router, (:-)(..), (<>), (.~)
    
    -- * Running routers
  , parse, unparse
  , parse1, unparse1
    
    -- * Constructing routers
    -- | The @constrN@ functions are helper functions to lift constructors of
    -- datatypes to routers. Their first argument is the constructor; their
    -- second argument is a (partial) destructor.
  , constr0, constr1, constr2, constr3
  , int, string, part, val, slash, lit
  , opt, many, some, duck, satisfy
  , nilP, consP, listP
  , leftP, rightP, eitherP
  , nothingP, justP, maybeP
  , pairP
  ) where

import Prelude hiding ((.), id)
import Control.Monad (mzero, mplus, guard)
import Control.Category
import Control.Arrow (first)
import Data.Monoid

infixr 8 <>
infixr 8 :-
infixr 9 .~

-- | Infix operator for 'mappend'.
(<>) :: Monoid m => m -> m -> m
(<>) = mappend

data Router a b = Router
  { ser :: b -> [(a, String)]
  , prs :: String -> [(a -> b, String)] }

data a :- b = a :- b deriving (Eq, Show)

hhead :: (a :- b) -> a
hhead (a :- _) = a

htail :: (a :- b) -> b
htail (_ :- b) = b

xmap :: (b -> a) -> (a -> b) -> Router r a -> Router r b
xmap f g (Router s p) = Router (s . f) ((fmap . fmap . first . fmap) g p)
  
instance Category Router where
  id = lit ""
  ~(Router sf pf) . ~(Router sg pg) = Router 
    (composeS (++) sf sg) 
    (composeP (.) pf pg)

(.~) :: Router a b -> Router b c -> Router a c
~(Router sf pf) .~ ~(Router sg pg) = Router 
  (composeS (flip (++)) sg sf)
  (composeP (flip (.)) pf pg)

composeS 
  :: (String -> String -> String)
  -> (a -> [(b, String)]) 
  -> (b -> [(c, String)]) 
  -> (a -> [(c, String)])
composeS op sf sg a = do
  (b, s) <- sf a
  (c, s') <- sg b
  return (c, s `op` s')

composeP
  :: (a -> b -> c)
  -> (String -> [(a, String)])
  -> (String -> [(b, String)])
  -> (String -> [(c, String)])
composeP op pf pg s = do
  (f, s') <- pf s
  (g, s'') <- pg s'
  return (f `op` g, s'')

instance Monoid (Router a b) where
  mempty = Router (const mzero) (const mzero)
  ~(Router sf pf) `mappend` ~(Router sg pg) = Router 
    (\s -> sf s `mplus` sg s)
    (\s -> pf s `mplus` pg s)

parse :: Router () a -> String -> [a]
parse p s = [ a () | (a, "") <- prs p s ]

parse1 :: Router () (a :- ()) -> String -> [a]
parse1 p = map hhead . parse p

unparse :: Router () a -> a -> [String]
unparse p = map snd . ser p

unparse1 :: Router () (a :- ()) -> a -> [String]
unparse1 p x = unparse p (x :- ())

maph :: (b -> a) -> (a -> b) -> Router i (a :- o) -> Router i (b :- o)
maph f g = xmap (\(h :- t) -> f h :- t) (\(h :- t) -> g h :- t)

opt :: Router r r -> Router r r
opt = (<> id)

many :: Router r r -> Router r r
many = opt . some

some :: Router r r -> Router r r
some p = p . many p

satisfy :: (Char -> Bool) -> Router r (Char :- r)
satisfy p = Router
  (\(c :- a) -> if (p c) then return (a, [c]) else mzero)
  (\s -> case s of 
    []     -> mzero
    (c:cs) -> if (p c) then return ((c :-), cs) else mzero)

char :: Router r (Char :- r)
char = satisfy (const True)

digitChar :: Router r (Char :- r)
digitChar = satisfy (\c -> c >= '0' && c <= '9')

digit :: Router r (Int :- r)
digit = maph (head . show) (read . (:[])) digitChar

push :: Eq h => h -> Router r (h :- r)
push h = Router 
  (\(h' :- t) -> do guard (h == h'); return (t, ""))
  (\s -> return ((h :-), s))

duck :: Router r1 r2 -> Router (h :- r1) (h :- r2)
duck r = Router
  (\(h :- t) -> map (first (h :-)) $ ser r t)
  (map (first (\f (h :- t) -> h :- f t)) . prs r)


nilP :: Router r ([a] :- r)
nilP = constr0 [] $ \x -> do [] <- x; Just ()

consP :: Router (a :- [a] :- r) ([a] :- r)
consP = constr2 (:) $ \x -> do a:as <- x; return (a, as)

listP :: (forall r. Router r (a :- r)) -> Router r ([a] :- r)
listP r = many (consP . r) . nilP


leftP :: Router (a :- r) (Either a b :- r)
leftP = constr1 Left $ \x -> do Left a <- x; return a

rightP :: Router (b :- r) (Either a b :- r)
rightP = constr1 Right $ \x -> do Right b <- x; return b

eitherP :: Router r (a :- r) -> Router r (b :- r) -> Router r (Either a b :- r)
eitherP l r = leftP . l <> rightP . r


nothingP :: Router r (Maybe a :- r)
nothingP = constr0 Nothing $ \x -> do Nothing <- x; Just ()

justP :: Router (a :- r) (Maybe a :- r)
justP = constr1 Just $ \x -> do Just a <- x; return a

maybeP :: Router r (a :- r) -> Router r (Maybe a :- r)
maybeP r = justP . r <> nothingP


pairP :: Router (f :- s :- r) ((f, s) :- r)
pairP = constr2 (,) id



-- | Routes a constant string.
lit :: String -> Router r r
lit l = Router
  (\b -> return (b, l))
  (\s -> let (s1, s2) = splitAt (length l) s in if s1 == l then return (id, s2) else mzero)

-- | Routes a slash.
slash :: Router r r
slash = lit "/"

-- | Routes any integer.
int :: Router r (Int :- r)
int = val

-- | Routes any string.
string :: Router r (String :- r)
string = Router
  (\(s :- r) -> return (r, s))
  (\s -> return ((s :-), ""))

-- | Routes part of a URL, i.e. a String not containing '/' or '?'.
part :: Router r (String :- r)
part = listP (satisfy (\c -> c /= '/' && c /= '?'))

-- | Routes any value that has a Show and Read instance.
val :: (Show a, Read a) => Router r (a :- r)
val = Router
  (\(a :- r) -> return (r, show a))
  (map (first (:-)) . reads)



-- | For example:
-- 
-- > nil :: Router r ([a] :- r)
-- > nil = constr0 [] $ \x -> do [] <- x; Just ()
constr0 :: o -> (Maybe o -> Maybe ()) -> Router r (o :- r)
constr0 c d = Router 
  (\(a :- t) -> maybe mzero (\_ -> return (t, "")) (d (return a)))
  (\s -> return ((c :-), s))

-- | For example:
--
-- > left :: Router (a :- r) (Either a b :- r)
-- > left = constr1 Left $ \x -> do Left a <- x; return a
constr1 :: (a -> o) -> (Maybe o -> Maybe a) -> Router (a :- r) (o :- r)
constr1 c d = Router
  (\(a :- t) -> maybe mzero (\a -> return (a :- t, "")) (d (return a)))
  (\s -> return (\(a :- t) -> c a :- t, s))

-- | For example:
--
-- > cons :: Router (a :- [a] :- r) ([a] :- r)
-- > cons = constr2 (:) $ \x -> do a:as <- x; return (a, as)
constr2 :: (a -> b -> o) -> (Maybe o -> Maybe (a, b)) ->
  Router (a :- b :- r) (o :- r)
constr2 c d = Router
  (\(a :- t) ->
    maybe mzero (\(a, b) -> return (a :- b :- t, "")) (d (return a)))
  (\s -> return (\(a :- b :- t) -> c a b :- t, s))

-- | For example:
--
-- > ifte :: Router (Bool :- Expr :- Expr :- r) (Expr :- r)
-- > ifte = constr3 IfThenElse $ \x -> do IfThenElse b t e <- x; return (b, t, e)
constr3 :: (a -> b -> c -> o) -> (Maybe o -> Maybe (a, b, c)) ->
  Router (a :- b :- c :- r) (o :- r)
constr3 c d = Router
  (\(a :- t) ->
    maybe mzero (\(a, b, c) -> return (a :- b :- c :- t, "")) (d (return a)))
  (\s -> return (\(i :- j :- k :- t) -> c i j k :- t, s))
