{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

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
  , pure, constr0, constr1, constr2, constr3
  , int, string, char, part, digit, val, (/), lit
  , opt, duck, satisfy, having, printAs
  , manyr, somer, chainr1 
  , manyl, somel, chainl1
  
  , nilP, consP, listP
  , leftP, rightP, eitherP
  , nothingP, justP, maybeP
  , pairP
  ) where

import Prelude hiding ((.), id)
import Control.Monad (mzero, mplus, guard)
import Control.Category
import Control.Arrow (first, second)
import Data.Monoid
import Data.Maybe (listToMaybe)
import Data.Char (isDigit)
import GHC.Exts

infixr 8 <>
infixr 8 :-
infixr 9 .~

-- | Infix operator for 'mappend'.
(<>) :: Monoid m => m -> m -> m
(<>) = mappend

data Router a b = Router
  { ser :: b -> [(String -> String, a)]
  , prs :: String -> [(a -> b, String)] }

instance a ~ b => IsString (Router a b) where
  fromString = lit

data a :- b = a :- b deriving (Eq, Show)

hhead :: (a :- b) -> a
hhead (a :- _) = a

htail :: (a :- b) -> b
htail (_ :- b) = b

xmap :: (b -> Maybe a) -> (a -> b) -> Router r a -> Router r b
xmap f g (Router s p) = Router (maybe mzero s . f) ((fmap . fmap . first . fmap) g p)
  
instance Category Router where
  id = lit ""
  ~(Router sf pf) . ~(Router sg pg) = Router 
    (compose (.) sf sg) 
    (compose (.) pf pg)

(.~) :: Router a b -> Router b c -> Router a c
~(Router sf pf) .~ ~(Router sg pg) = Router 
  (compose (flip (.)) sg sf)
  (compose (flip (.)) pf pg)

compose
  :: (a -> b -> c)
  -> (i -> [(a, j)])
  -> (j -> [(b, k)])
  -> (i -> [(c, k)])
compose op mf mg s = do
  (f, s') <- mf s
  (g, s'') <- mg s'
  return (f `op` g, s'')

instance Monoid (Router a b) where
  mempty = Router (const mzero) (const mzero)
  ~(Router sf pf) `mappend` ~(Router sg pg) = Router 
    (\s -> sg s `mplus` sf s)
    (\s -> pf s `mplus` pg s)

parse :: Router () a -> String -> [a]
parse p s = [ a () | (a, "") <- prs p s ]

parse1 :: Router () (a :- ()) -> String -> Maybe a
parse1 p = listToMaybe . map hhead . parse p

unparse :: Router () a -> a -> [String]
unparse p = map (($ "") . fst) . ser p

unparse1 :: Router () (a :- ()) -> a -> Maybe String
unparse1 p = listToMaybe . unparse p . (:- ())

maph :: (b -> Maybe a) -> (a -> b) -> Router i (a :- o) -> Router i (b :- o)
maph f g = xmap (\(h :- t) -> maybe Nothing (Just . (:- t)) $ f h) (\(h :- t) -> g h :- t)

opt :: Router r r -> Router r r
opt = (<> id)

manyr :: Router r r -> Router r r
manyr = opt . somer

somer :: Router r r -> Router r r
somer p = p . manyr p

chainr1 :: (forall r. Router r (a :- r)) -> (forall r. Router (a :- a :- r) (a :- r)) -> Router r (a :- r)
chainr1 p op = manyr (p .~ op) . p

manyl :: Router r r -> Router r r
manyl = opt . somel

somel :: Router r r -> Router r r
somel p = p .~ manyl p

chainl1 :: (forall r. Router r (a :- r)) -> (forall r. Router (a :- a :- r) (a :- r)) -> Router r (a :- r)
chainl1 p op = p .~ manyl (op . duck p)


apply :: Router ((b -> a) :- r) ((a -> b) :- r) -> Router (a :- r) (b :- r)
apply r = Router
  (\(b :- t) -> map (second (\(f :- r) -> f b :- r)) $ ser r (const b :- t))
  (\s -> map (first (\f (a :- r) -> let (g :- t) = f (const a :- r) in g a :- t)) $ prs r s)



having :: (forall r. Router r (a :- r)) -> (a -> Bool) -> Router r (a :- r)
having r p = Router
  (\(a :- t) -> if (p a) then ser r (a :- t) else mzero)
  (\s -> map (first ((:-) . hhead . ($ ()))) $ filter (p . hhead . ($ ()) . fst) $ prs r s)

satisfy :: (Char -> Bool) -> Router r (Char :- r)
satisfy p = Router
  (\(c :- a) -> if (p c) then return ((c :), a) else mzero)
  (\s -> case s of 
    []     -> mzero
    (c:cs) -> if (p c) then return ((c :-), cs) else mzero)

char :: Router r (Char :- r)
char = satisfy (const True)

digit :: Router r (Int :- r)
digit = maph ((\a -> do [h] <- Just a; Just h) . show) (read . (:[])) $ satisfy isDigit

push :: Eq h => h -> Router r (h :- r)
push h = Router 
  (\(h' :- t) -> do guard (h == h'); return (id, t))
  (\s -> return ((h :-), s))

duck :: Router r1 r2 -> Router (h :- r1) (h :- r2)
duck r = Router
  (\(h :- t) -> map (second (h :-)) $ ser r t)
  (map (first (\f (h :- t) -> h :- f t)) . prs r)

printAs :: Router a b -> String -> Router a b
printAs r s = Router
  (\b -> case ser r b of
           [] -> []
           (_, a) : _ -> [((s ++), a)])
  (prs r)


nilP :: Router r ([a] :- r)
nilP = constr0 [] $ \x -> do [] <- x; Just ()

consP :: Router (a :- [a] :- r) ([a] :- r)
consP = constr2 (:) $ \x -> do a:as <- x; return (a, as)

listP :: (forall r. Router r (a :- r)) -> Router r ([a] :- r)
listP r = manyr (consP . r) . nilP


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
  (\b -> return ((l ++), b))
  (\s -> let (s1, s2) = splitAt (length l) s in if s1 == l then return (id, s2) else mzero)

-- | @p / q@ is equivalent to @p . "/" . q@.
infixr 9 /
(/) :: Router b c -> Router a b -> Router a c
f / g = f . lit "/" . g

-- | Routes any integer.
int :: Router r (Int :- r)
int = val

-- | Routes any string.
string :: Router r (String :- r)
string = Router
  (\(s :- r) -> return ((s ++), r))
  (\s -> return ((s :-), ""))

-- | Routes part of a URL, i.e. a String not containing '/' or '?'.
part :: Router r (String :- r)
part = listP (satisfy (\c -> c /= '/' && c /= '?'))

-- | Routes any value that has a Show and Read instance.
val :: (Show a, Read a) => Router r (a :- r)
val = Router
  (\(a :- r) -> return ((show a ++), r))
  (map (first (:-)) . reads)



-- | For example:
-- 
-- > nil :: Router r ([a] :- r)
-- > nil = constr0 [] $ \x -> do [] <- x; Just ()
constr0 :: o -> (Maybe o -> Maybe ()) -> Router r (o :- r)
constr0 c d = Router 
  (\(a :- t) -> maybe mzero (\_ -> return (id, t)) (d (return a)))
  (\s -> return ((c :-), s))

-- | For example:
--
-- > left :: Router (a :- r) (Either a b :- r)
-- > left = constr1 Left $ \x -> do Left a <- x; return a
constr1 :: (a -> o) -> (Maybe o -> Maybe a) -> Router (a :- r) (o :- r)
constr1 c d = Router
  (\(a :- t) -> maybe mzero (\a -> return (id, a :- t)) (d (return a)))
  (\s -> return (\(a :- t) -> c a :- t, s))

-- | For example:
--
-- > cons :: Router (a :- [a] :- r) ([a] :- r)
-- > cons = constr2 (:) $ \x -> do a:as <- x; return (a, as)
constr2 :: (a -> b -> o) -> (Maybe o -> Maybe (a, b)) ->
  Router (a :- b :- r) (o :- r)
constr2 c d = Router
  (\(a :- t) ->
    maybe mzero (\(a, b) -> return (id, a :- b :- t)) (d (return a)))
  (\s -> return (\(a :- b :- t) -> c a b :- t, s))

-- | For example:
--
-- > ifte :: Router (Bool :- Expr :- Expr :- r) (Expr :- r)
-- > ifte = constr3 IfThenElse $ \x -> do IfThenElse b t e <- x; return (b, t, e)
constr3 :: (a -> b -> c -> o) -> (Maybe o -> Maybe (a, b, c)) ->
  Router (a :- b :- c :- r) (o :- r)
constr3 c d = Router
  (\(a :- t) ->
    maybe mzero (\(a, b, c) -> return (id, a :- b :- c :- t)) (d (return a)))
  (\s -> return (\(i :- j :- k :- t) -> c i j k :- t, s))

-- | Lift a constructor-destructor pair to a pure router.
pure :: (a -> b) -> (b -> Maybe a) -> Router a b
pure f g = Router g' f'
  where
    f' s = [(f, s)]
    g' b =
      case g b of
        Nothing -> []
        Just a  -> [(id, a)]
