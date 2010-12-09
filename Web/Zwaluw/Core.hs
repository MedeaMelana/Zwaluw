{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Web.Zwaluw.Core (
    -- * Types
    Router(..), (:-)(..), (.~)
    
    -- * Running routers
  , parse, unparse
  , parse1, unparse1
    
  , xmap, pure, lit, xmaph
  , hhead, htail, hdMap, hdTraverse, pop, arg
  , val, duck, printAs
  ) where

import Prelude hiding ((.), id, (/))
import Control.Category
import Data.Monoid

import Control.Monad (mzero, mplus)
import Control.Arrow (first, second)
import Data.Maybe (listToMaybe)
import GHC.Exts


infixr 8 :-
infixr 9 .~


-- | A @Router a b@ takes an @a@ to parse a URL and results in @b@ if parsing succeeds.
--   And it takes a @b@ to serialize to a URL and results in @a@ if serializing succeeds.
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

-- | Reverse composition, but with the side effects still in left-to-right order.
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
    (\s -> sf s `mplus` sg s)

instance a ~ b => IsString (Router a b) where
  fromString = lit


-- | Map over routers.
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


-- | A stack datatype. Just a better looking tuple.
data a :- b = a :- b deriving (Eq, Show)

-- | Stack destructor.
pop :: (a -> b -> r) -> (a :- b) -> r
pop f (a :- b) = f a b

-- | Get the top of the stack.
hhead :: (a :- b) -> a
hhead (a :- _) = a

-- | Get the stack with the top popped.
htail :: (a :- b) -> b
htail (_ :- b) = b

-- | Applicative traversal over the top of the stack.
hdTraverse :: Functor f => (a -> f b) -> a :- t -> f (b :- t)
hdTraverse f (a :- t) = fmap (:- t) (f a)

arg :: (ty -> r -> s) -> (a -> ty) -> (a :- r) -> s
arg c f = pop (c . f)

-- | Map over the top of the stack.
hdMap :: (a1 -> a2) -> (a1 :- b) -> (a2 :- b)
hdMap = arg (:-)

-- | Like "xmap", but only maps over the top of the stack.
xmaph :: (a -> b) -> (b -> Maybe a) -> Router i (a :- o) -> Router i (b :- o)
xmaph f g = xmap (hdMap f) (hdTraverse g)


-- | Build a router for a value given all the ways to parse and serialize it.
val :: (String -> [(a, String)]) -> (a -> [String -> String]) -> Router r (a :- r)
val rs ss = Router
  (map (first (:-)) . rs)
  (\(a :- r) -> map (\f -> (f, r)) (ss a))

-- | Convert a router to do what it does on the tail of the stack.
duck :: Router r1 r2 -> Router (h :- r1) (h :- r2)
duck r = Router
  (map (first (\f (h :- t) -> h :- f t)) . prs r)
  (\(h :- t) -> map (second (h :-)) $ ser r t)

-- | @r \`printAs\` s@ uses ther serializer of @r@ to test if serializing succeeds,
--   and if it does, instead serializes as @s@. 
printAs :: Router a b -> String -> Router a b
printAs r s = r { ser = map (first (const (s ++))) . take 1 . ser r }


-- | Give all possible parses.
parse :: Router () a -> String -> [a]
parse p s = [ a () | (a, "") <- prs p s ]

-- | Give the first parse, for Routers with a parser that yields just one value.
parse1 :: Router () (a :- ()) -> String -> Maybe a
parse1 p = listToMaybe . map hhead . parse p

-- | Give all possible serializations.
unparse :: Router () a -> a -> [String]
unparse p = map (($ "") . fst) . ser p

-- | Give the first serialization, for Routers with a serializer that needs just one value.
unparse1 :: Router () (a :- ()) -> a -> Maybe String
unparse1 p = listToMaybe . unparse p . (:- ())
