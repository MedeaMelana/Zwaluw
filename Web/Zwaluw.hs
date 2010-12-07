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
  , pure, hdMap, hdTraverse
  , int, string, char, part, digit, val, readshow, (/), lit
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



having :: (forall r. Router r (a :- r)) -> (a -> Bool) -> Router r (a :- r)
having r p = val
  (filter (p . fst) . map (first (hhead . ($ ()))) . prs r)
  (\a -> if p a then map fst (ser r (a :- ())) else [])

satisfy :: (Char -> Bool) -> Router r (Char :- r)
satisfy p = val
  (\s -> [(c, cs) | c:cs <- [s], p c])
  (\c -> [(c :) | p c])

char :: Router r (Char :- r)
char = satisfy (const True)

digit :: Router r (Int :- r)
digit = maph ((\a -> do [h] <- Just a; Just h) . show) (read . (:[])) $ satisfy isDigit


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
int = readshow

-- | Routes any string.
string :: Router r (String :- r)
string = val (\s -> [(s, "")]) (return . (++))

-- | Routes part of a URL, i.e. a String not containing '/' or '?'.
part :: Router r (String :- r)
part = rList (satisfy (\c -> c /= '/' && c /= '?'))

-- | Routes any value that has a Show and Read instance.
readshow :: (Show a, Read a) => Router r (a :- r)
readshow = val reads (return . shows)
