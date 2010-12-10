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
    
    -- * Router combinators
  , pure, xmap, xmaph
  , val, readshow, lit
  , opt, duck, satisfy, rFilter, printAs
  , manyr, somer, chainr, chainr1 
  , manyl, somel, chainl, chainl1
  
    -- * Built-in routers
  , int, string, char, digit, hexDigit
  , (/), part
  
  , rNil, rCons, rList
  , rPair
  , rLeft, rRight, rEither
  , rNothing, rJust, rMaybe
  ) where

import Prelude hiding ((.), id, (/))
import Control.Monad (guard)
import Control.Category
import Data.Monoid
import Data.Char (isDigit, isHexDigit, intToDigit, digitToInt)

import Web.Zwaluw.Core
import Web.Zwaluw.TH


infixr 8 <>

-- | Infix operator for 'mappend'.
(<>) :: Monoid m => m -> m -> m
(<>) = mappend


-- | Make a router optional.
opt :: Router r r -> Router r r
opt = (id <>)

-- | Repeat a router zero or more times, combining the results from left to right.
manyr :: Router r r -> Router r r
manyr = opt . somer

-- | Repeat a router one or more times, combining the results from left to right.
somer :: Router r r -> Router r r
somer p = p . manyr p

-- | @chainr p op@ repeats @p@ zero or more times, separated by @op@. 
--   The result is a right associative fold of the results of @p@ with the results of @op@.
chainr :: Router r r -> Router r r -> Router r r
chainr p op = opt (manyr (p .~ op) . p)

-- | @chainr1 p op@ repeats @p@ one or more times, separated by @op@. 
--   The result is a right associative fold of the results of @p@ with the results of @op@.
chainr1 :: (forall r. Router r (a :- r)) -> (forall r. Router (a :- a :- r) (a :- r)) -> forall r. Router r (a :- r)
chainr1 p op = manyr (p .~ op) . p

-- | Repeat a router zero or more times, combining the results from right to left.
manyl :: Router r r -> Router r r
manyl = opt . somel

-- | Repeat a router one or more times, combining the results from right to left.
somel :: Router r r -> Router r r
somel p = p .~ manyl p

-- | @chainl1 p op@ repeats @p@ zero or more times, separated by @op@. 
--   The result is a left associative fold of the results of @p@ with the results of @op@.
chainl :: Router r r -> Router r r -> Router r r
chainl p op = opt (p .~ manyl (op . p))

-- | @chainl1 p op@ repeats @p@ one or more times, separated by @op@. 
--   The result is a left associative fold of the results of @p@ with the results of @op@.
chainl1 :: (forall r. Router r (a :- r)) -> (forall r. Router (a :- a :- r) (a :- r)) -> forall r. Router r (a :- r)
chainl1 p op = p .~ manyl (op . duck p)

-- | Filtering on routers.
rFilter :: (a -> Bool) -> Router () (a :- ()) -> Router r (a :- r)
rFilter p r = val
  (\s -> [ (a, s') | (f, s') <- prs r s, let a = hhead (f ()), p a ])
  (\a -> [ f | p a, (f, _) <- ser r (a :- ()) ])



-- | Routes any value that has a Show and Read instance.
readshow :: (Show a, Read a) => Router r (a :- r)
readshow = val reads (return . shows)

-- | Routes any integer.
int :: Router r (Int :- r)
int = readshow

-- | Routes any string.
string :: Router r (String :- r)
string = val (\s -> [(s, "")]) (return . (++))

-- | Routes one character satisfying the given predicate.
satisfy :: (Char -> Bool) -> Router r (Char :- r)
satisfy p = val
  (\s -> [ (c, cs) | c:cs <- [s], p c ])
  (\c -> [ (c :) | p c ])

-- | Routes one character.
char :: Router r (Char :- r)
char = satisfy (const True)

-- | Routes one decimal digit.
digit :: Router r (Int :- r)
digit = xmaph digitToInt (\i -> guard (i >= 0 && i < 10) >> Just (intToDigit i)) (satisfy isDigit)

-- | Routes one hexadecimal digit.
hexDigit :: Router r (Int :- r)
hexDigit = xmaph digitToInt (\i -> guard (i >= 0 && i < 16) >> Just (intToDigit i)) (satisfy isHexDigit)

infixr 9 /
-- | @p \/ q@ is equivalent to @p . \"\/\" . q@.
(/) :: Router b c -> Router a b -> Router a c
(/) f g = f . lit "/" . g

-- | Routes part of a URL, i.e. a String not containing @\'\/\'@ or @\'\?\'@.
part :: Router r (String :- r)
part = rList (satisfy (\c -> c /= '/' && c /= '?'))


rNil :: Router r ([a] :- r)
rNil = pure ([] :-) $ \(xs :- t) -> do [] <- Just xs; Just t

rCons :: Router (a :- [a] :- r) ([a] :- r)
rCons = pure (arg (arg (:-)) (:)) $ \(xs :- t) -> do a:as <- Just xs; Just (a :- as :- t)

-- | Converts a router for a value @a@ to a router for a list of @a@.
rList :: (forall r. Router r (a :- r)) -> forall r. Router r ([a] :- r)
rList r = manyr (rCons . r) . rNil

rPair :: Router (f :- s :- r) ((f, s) :- r)
rPair = pure (arg (arg (:-)) (,)) $ \(ab :- t) -> do (a,b) <- Just ab; Just (a :- b :- t)

$(deriveRouters ''Either)
rLeft  :: Router (a :- r) (Either a b :- r)
rRight :: Router (b :- r) (Either a b :- r)

-- | Combines a router for a value @a@ and a router for a value @b@ into a router for @Either a b@.
rEither :: Router r (a :- r) -> Router r (b :- r) -> Router r (Either a b :- r)
rEither l r = rLeft . l <> rRight . r

$(deriveRouters ''Maybe)
rNothing :: Router       r  (Maybe a :- r)
rJust    :: Router (a :- r) (Maybe a :- r)

-- | Converts a router for a value @a@ to a router for a @Maybe a@.
rMaybe :: Router r (a :- r) -> Router r (Maybe a :- r)
rMaybe r = rJust . r <> rNothing