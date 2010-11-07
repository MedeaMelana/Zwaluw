{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Web.Zwaluw where

import Prelude hiding ((.), id)
import Control.Monad
import Control.Category
import Control.Arrow (first)
import Data.Monoid

infixr 8 <>
infixr 8 :-

(<>) :: Monoid m => m -> m -> m
(<>) = mappend

data P a b = P
  { ser :: b -> [(a, String)]
  , prs :: String -> [(a -> b, String)] }

data a :- b = a :- b deriving (Eq, Show)

xmap :: (b -> a) -> (a -> b) -> P r a -> P r b
xmap f g (P s p) = P (s . f) ((fmap . liftM . first . fmap) g p)
  
instance Category (P) where
  id = lit ""
  P sf pf . P sg pg = P 
    (\a -> do
        (b, s) <- sf a
        (c, s') <- sg b
        return (c, s ++ s'))
    (\s -> do
        (f, s') <- pf s
        (g, s'') <- pg s'
        return (f . g, s''))

instance Monoid (P a b) where
  mempty = P (const mzero) (const mzero)
  P sf pf `mappend` P sg pg = P 
    (\s -> sf s `mplus` sg s)
    (\s -> pf s `mplus` pg s)

parse :: P () a -> String -> [a]
parse p = concatMap (\(a, s) -> if (s == "") then [a ()] else []) . prs p

unparse :: P () a -> a -> [String]
unparse p = map snd . ser p

maph :: (b -> a) -> (a -> b) -> P i (a :- o) -> P i (b :- o)
maph f g = xmap (\(h :- t) -> f h :- t) (\(h :- t) -> g h :- t)

opt :: Eq a => a -> P r (a :- r) -> P r (a :- r)
opt a p = p <> push a

nil :: P r ([a] :- r)
nil = constr0 [] $ \x -> do [] <- Just x; Just ()

cons :: P (a :- [a] :- r) ([a] :- r)
cons = constr2 (:) $ \x -> do a:as <- Just x; Just (a, as)

many :: Eq a => (forall r. P r (a :- r)) -> P r ([a] :- r)
many p = nil <> many1 p

many1 :: Eq a => (forall r. P r (a :- r)) -> P r ([a] :- r)
many1 p = cons . p . many p

satisfy :: (Char -> Bool) -> P r (Char :- r)
satisfy p = P
  (\(c :- a) -> if (p c) then return (a, [c]) else mzero)
  (\s -> case s of 
    []     -> mzero
    (c:cs) -> if (p c) then return ((c :-), cs) else mzero)

char :: P r (Char :- r)
char = satisfy (const True)

digitChar :: P r (Char :- r)
digitChar = satisfy (\c -> c >= '0' && c <= '9')

digit :: P r (Int :- r)
digit = maph (head . show) (read . (:[])) digitChar

int :: P r (Int :- r)
-- int = maph show read $ many1 digitChar
int = digit

lit :: String -> P r r
lit l = P
  (\b -> return (b, l))
  (\s -> let (s1, s2) = splitAt (length l) s in if s1 == l then return (id, s2) else mzero)

push :: Eq h => h -> P r (h :- r)
push h = P 
  (\(h' :- t) -> do guard (h == h'); return (t, ""))
  (\s -> return ((h :-), s))

left :: P (a :- r) (Either a b :- r)
left = constr1 Left $ \x -> do Left a <- Just x; return a

right :: P (b :- r) (Either a b :- r)
right = constr1 Right $ \x -> do Right b <- Just x; return b
 
eitherP :: P r (a :- r) -> P r (b :- r) -> P r (Either a b :- r)
eitherP l r = left . l <> right . r

slash :: P r r
slash = lit "/"

constr0 :: o -> (o -> Maybe ()) -> P r (o :- r)
constr0 c d = P 
  (\(a :- t) -> maybe mzero (\_ -> return (t, "")) (d a))
  (\s -> return ((c :-), s))

constr1 :: (a -> o) -> (o -> Maybe a) -> P (a :- r) (o :- r)
constr1 c d = P
  (\(a :- t) -> maybe mzero (\a -> return (a :- t, "")) (d a))
  (\s -> return (\(a :- t) -> c a :- t, s))

constr2 :: (a -> b -> o) -> (o -> Maybe (a, b)) -> P (a :- b :- r) (o :- r)
constr2 c d = P
  (\(a :- t) -> maybe mzero (\(a, b) -> return (a :- b :- t, "")) (d a))
  (\s -> return (\(a :- b :- t) -> c a b :- t, s))
