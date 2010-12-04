{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

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
  , pure, hdMap, hdTraverse
  , int, string, char, part, digit, val, (/), lit
  , opt, duck, satisfy, having, printAs
  , manyr, somer, chainr1 
  , manyl, somel, chainl1
  
  , rNil, rCons, rList
  , rPair
  , rLeft, rRight, rEither
  , rNothing, rJust, rMaybe
  ) where

import Prelude hiding ((.), id, (/))
import Control.Monad (mzero, mplus, guard)
import Control.Category
import Control.Arrow (first, second)
import Data.Monoid
import Data.Char (isDigit)
import GHC.Exts

import Web.Zwaluw.Core
import Web.Zwaluw.TH


infixr 8 <>

-- | Infix operator for 'mappend'.
(<>) :: Monoid m => m -> m -> m
(<>) = mappend

instance a ~ b => IsString (Router a b) where
  fromString = lit


maph :: (b -> Maybe a) -> (a -> b) -> Router i (a :- o) -> Router i (b :- o)
maph f g = xmap (\(h :- t) -> g h :- t) (\(h :- t) -> maybe Nothing (Just . (:- t)) $ f h)

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
  (\s -> map (first (\f (a :- r) -> let (g :- t) = f (const a :- r) in g a :- t)) $ prs r s)
  (\(b :- t) -> map (second (\(f :- r) -> f b :- r)) $ ser r (const b :- t))



having :: (forall r. Router r (a :- r)) -> (a -> Bool) -> Router r (a :- r)
having r p = Router
  (\s -> map (first ((:-) . hhead . ($ ()))) $ filter (p . hhead . ($ ()) . fst) $ prs r s)
  (\(a :- t) -> if (p a) then ser r (a :- t) else mzero)

satisfy :: (Char -> Bool) -> Router r (Char :- r)
satisfy p = Router
  (\s -> case s of 
    []     -> mzero
    (c:cs) -> if (p c) then return ((c :-), cs) else mzero)
  (\(c :- a) -> if (p c) then return ((c :), a) else mzero)

char :: Router r (Char :- r)
char = satisfy (const True)

digit :: Router r (Int :- r)
digit = maph ((\a -> do [h] <- Just a; Just h) . show) (read . (:[])) $ satisfy isDigit

push :: Eq h => h -> Router r (h :- r)
push h = Router 
  (\s -> return ((h :-), s))
  (\(h' :- t) -> do guard (h == h'); return (id, t))

duck :: Router r1 r2 -> Router (h :- r1) (h :- r2)
duck r = Router
  (map (first (\f (h :- t) -> h :- f t)) . prs r)
  (\(h :- t) -> map (second (h :-)) $ ser r t)

printAs :: Router a b -> String -> Router a b
printAs r s = Router
  (prs r)
  (\b -> case ser r b of
           [] -> []
           (_, a) : _ -> [((s ++), a)])


rNil :: Router r ([a] :- r)
rNil = pure ([] :-) $ \(xs :- t) -> do [] <- Just xs; Just t

rCons :: Router (a :- [a] :- r) ([a] :- r)
rCons = pure (arg (arg (:-)) (:)) $ \(xs :- t) -> do a:as <- Just xs; Just (a :- as :- t)

rList :: (forall r. Router r (a :- r)) -> Router r ([a] :- r)
rList r = manyr (rCons . r) . rNil

rPair :: Router (f :- s :- r) ((f, s) :- r)
rPair = pure (arg (arg (:-)) (,)) $ \(ab :- t) -> do (a,b) <- Just ab; Just (a :- b :- t)

$(deriveRouters ''Either)

rEither :: Router r (a :- r) -> Router r (b :- r) -> Router r (Either a b :- r)
rEither l r = rLeft . l <> rRight . r

$(deriveRouters ''Maybe)

rMaybe :: Router r (a :- r) -> Router r (Maybe a :- r)
rMaybe r = rJust . r <> rNothing



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
  (\s -> return ((s :-), ""))
  (\(s :- r) -> return ((s ++), r))

-- | Routes part of a URL, i.e. a String not containing '/' or '?'.
part :: Router r (String :- r)
part = rList (satisfy (\c -> c /= '/' && c /= '?'))

-- | Routes any value that has a Show and Read instance.
val :: (Show a, Read a) => Router r (a :- r)
val = Router
  (map (first (:-)) . reads)
  (\(a :- r) -> return ((show a ++), r))
