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

xmap :: (b -> a) -> (a -> b) -> P t a -> P t b
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

nil :: Constr0 [a]
nil = constr0 [] $ \x -> do [] <- Just x; Just ()

cons :: Constr2 [a] a [a]
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

push :: Eq h => h -> P t (h :- t)
push h = P 
  (\(h' :- t) -> do guard (h == h'); return (t, ""))
  (\s -> return ((h :-), s))

left :: Constr1 (Either a b) a
left = constr1 Left $ \x -> do Left a <- Just x; return a

right :: Constr1 (Either a b) b
right = constr1 Right $ \x -> do Right b <- Just x; return b
 
eitherP :: P t (a :- t) -> P t (b :- t) -> P t (Either a b :- t)
eitherP l r = left . l <> right . r

slash :: P r r
slash = lit "/"

type Constr0 o = forall t. P t (o :- t)
constr0 :: o -> (o -> Maybe ()) -> Constr0 o
constr0 c d = P 
  (\(a :- t) -> maybe mzero (\_ -> return (t, "")) (d a))
  (\s -> return ((c :-), s))

type Constr1 o a = forall t. P (a :- t) (o :- t) 
constr1 :: (a -> o) -> (o -> Maybe a) -> Constr1 o a
constr1 c d = P
  (\(a :- t) -> maybe mzero (\a -> return (a :- t, "")) (d a))
  (\s -> return (\(a :- t) -> c a :- t, s))

type Constr2 o a b = forall t. P (a :- b :- t) (o :- t) 
constr2 :: (a -> b -> o) -> (o -> Maybe (a, b)) -> Constr2 o a b
constr2 c d = P
  (\(a :- t) -> maybe mzero (\(a, b) -> return (a :- b :- t, "")) (d a))
  (\s -> return (\(a :- b :- t) -> c a b :- t, s))
