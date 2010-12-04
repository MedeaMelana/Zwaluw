{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Web.Zwaluw.Core (
    -- * Types
    Router(..), (:-)(..), (.~)
    
    -- * Running routers
  , parse, unparse
  , parse1, unparse1
    
  , xmap, pure, lit
  , hhead, htail, hdMap, hdTraverse, arg
  ) where

import Prelude hiding ((.), id, (/))
import Control.Category
import Data.Monoid

import Control.Monad (mzero, mplus)
import Control.Arrow (first)
import Data.Maybe (listToMaybe)


infixr 8 :-
infixr 9 .~



data Router a b = Router
  { prs :: String -> [(a -> b, String)]
  , ser :: b -> [(String -> String, a)] }

instance Category Router where
  id = Router
    (\x -> [(id, x)])
    (\x -> [(id, x)])
  ~(Router pf sf) . ~(Router pg sg) = Router 
    (compose (.) pf pg)
    (compose (.) sf sg) 

(.~) :: Router a b -> Router b c -> Router a c
~(Router pf sf) .~ ~(Router pg sg) = Router 
  (compose (flip (.)) pf pg)
  (compose (flip (.)) sg sf)

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
  mempty = Router 
    (const mzero)
    (const mzero)
  ~(Router pf sf) `mappend` ~(Router pg sg) = Router 
    (\s -> pf s `mplus` pg s)
    (\s -> sg s `mplus` sf s)

xmap :: (a -> b) -> (b -> Maybe a) -> Router r a -> Router r b
xmap f g (Router p s) = Router ((fmap . fmap . first . fmap) f p) (maybe mzero s . g)
  
-- | Lift a constructor-destructor pair to a pure router.
pure :: (a -> b) -> (b -> Maybe a) -> Router a b
pure f g = xmap f g id

-- | Routes a constant string.
lit :: String -> Router r r
lit l = Router
  (\s -> let (s1, s2) = splitAt (length l) s in if s1 == l then return (id, s2) else mzero)
  (\b -> return ((l ++), b))


data a :- b = a :- b deriving (Eq, Show)

hhead :: (a :- b) -> a
hhead (a :- _) = a

htail :: (a :- b) -> b
htail (_ :- b) = b

hdMap :: (a1 -> a2) -> (a1 :- b) -> (a2 :- b)
hdMap f (a :- b) = f a :- b

hdTraverse :: Functor f => (a -> f b) -> a :- t -> f (b :- t)
hdTraverse f (a :- t) = fmap (:- t) (f a)

arg :: (ty -> r -> s) -> (a -> ty) -> (a :- r) -> s
arg c f (x :- r) = c (f x) r




parse :: Router () a -> String -> [a]
parse p s = [ a () | (a, "") <- prs p s ]

parse1 :: Router () (a :- ()) -> String -> Maybe a
parse1 p = listToMaybe . map hhead . parse p

unparse :: Router () a -> a -> [String]
unparse p = map (($ "") . fst) . ser p

unparse1 :: Router () (a :- ()) -> a -> Maybe String
unparse1 p = listToMaybe . unparse p . (:- ())
