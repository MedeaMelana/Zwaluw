{-# LANGUAGE RankNTypes #-}

import Prelude hiding ((.), id)
import Control.Monad
import Control.Category
import Control.Arrow (first, second)
import Data.Monoid

infixr 8 <>

(<>) :: Monoid m => m -> m -> m
(<>) = mappend

data P m a b = P
  { ser :: b -> m (a, String)
  , prs :: String -> m (a -> b, String) }

xmap :: MonadPlus m => (b -> a) -> (a -> b) -> P m t a -> P m t b
xmap f g (P s p) = P (s . f) ((fmap . liftM . first . fmap) g p)
  
instance MonadPlus m => Category (P m) where
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

instance MonadPlus m => Monoid (P m a b) where
  mempty = P (const mzero) (const mzero)
  P sf pf `mappend` P sg pg = P 
    (\s -> sf s `mplus` sg s)
    (\s -> pf s `mplus` pg s)

parse :: P [] () a -> String -> [a]
parse p = concatMap (\(a, s) -> if (s == "") then [a ()] else []) . prs p

unparse :: P [] () a -> a -> [String]
unparse p = map snd . ser p

maph :: MonadPlus m => (b -> a) -> (a -> b) -> P m i (a, o) -> P m i (b, o)
maph f g = xmap (first f) (first g)

opt :: (MonadPlus m, Eq a) => a -> P m r (a, r) -> P m r (a, r)
opt a p = p <> push a

many :: (MonadPlus m, Eq a) => (forall r. P m r (a, r)) -> P m r ([a], r)
many p = nil <> many1 p

nil :: MonadPlus m => Constr0 m t [a]
nil = constr0 [] $ \x -> do [] <- Just x; Just ()

cons :: MonadPlus m => Constr2 m t [a] a [a]
cons = constr2 (:) $ \x -> do a:as <- Just x; Just (a, as)

many1 :: (MonadPlus m, Eq a) => (forall r. P m r (a, r)) -> P m r ([a], r)
many1 p = cons . p . many p

satisfy :: MonadPlus m => (Char -> Bool) -> P m r (Char, r)
satisfy p = P
  (\(c, a) -> if (p c) then return (a, [c]) else mzero)
  (\s -> case s of 
    []     -> mzero
    (c:cs) -> if (p c) then return (\a -> (c, a), cs) else mzero)

char :: MonadPlus m => P m r (Char, r)
char = satisfy (const True)

digitChar :: MonadPlus m => P m r (Char, r)
digitChar = satisfy (\c -> c >= '0' && c <= '9')

digit :: MonadPlus m => P m r (Int, r)
digit = maph (head . show) (read . (:[])) digitChar

int :: MonadPlus m => P m r (Int, r)
-- int = maph show read $ many1 digitChar
int = digit

lit :: MonadPlus m => String -> P m r r
lit l = P
  (\b -> return (b, l))
  (\s -> let (s1, s2) = splitAt (length l) s in if s1 == l then return (id, s2) else mzero)

push :: (MonadPlus m, Eq h) => h -> P m t (h, t)
push h = P 
  (\(h', t) -> do guard (h == h'); return (t, ""))
  (\s -> return (\t -> (h, t), s))

left :: MonadPlus m => Constr1 m t (Either a b) a
left = constr1 Left $ \x -> do Left a <- Just x; return a

right :: MonadPlus m => Constr1 m t (Either a b) b
right = constr1 Right $ \x -> do Right b <- Just x; return b
 
eitherP :: MonadPlus m => P m t (a, t) -> P m t (b, t) -> P m t (Either a b, t)
eitherP l r = left . l <> right . r


type Constr0 m t o = P m t (o, t)
constr0 :: MonadPlus m => o -> (o -> Maybe ()) -> Constr0 m t o
constr0 c d = P 
  (\(a, t) -> maybe mzero (\_ -> return (t, "")) (d a))
  (\s -> return (\t -> (c, t), s))

type Constr1 m t o a = P m (a, t) (o, t) 
constr1 :: MonadPlus m => (a -> o) -> (o -> Maybe a) -> Constr1 m t o a
constr1 c d = P
  (\(a, t) -> maybe mzero (\a -> return ((a, t), "")) (d a))
  (\s -> return (\(a, t) -> (c a, t), s))

type Constr2 m t o a b = P m (a, (b, t)) (o, t) 
constr2 :: MonadPlus m => (a -> b -> o) -> (o -> Maybe (a, b)) -> Constr2 m t o a b
constr2 c d = P
  (\(a, t) -> maybe mzero (\(a, b) -> return ((a, (b, t)), "")) (d a))
  (\s -> return (\(a, (b, t)) -> (c a b, t), s))



data Sitemap
   = Home
   | Range Int Int
   | CaseOverview
   | CaseDetail Int
   deriving (Eq, Show)

home :: MonadPlus m => Constr0 m t Sitemap
home = constr0 Home $ \a -> do Home <- Just a; Just ()

caseOverview :: MonadPlus m => Constr0 m t Sitemap
caseOverview = constr0 CaseOverview $ \a -> do CaseOverview <- Just a; Just ()

caseDetail :: MonadPlus m => Constr1 m t Sitemap Int
caseDetail = constr1 CaseDetail $ \a -> do CaseDetail i <- Just a; Just i

range :: MonadPlus m => Constr2 m t Sitemap Int Int
range = constr2 Range $ \a -> do Range l u <- Just a; Just (l, u)

slash :: MonadPlus m => P m r r
slash = lit "/"

url :: MonadPlus m => P m t (Sitemap, t)
url = 
  slash . 
  (  home . lit "home"
  <> lit "cases" .
       (  caseOverview
       <> caseDetail . slash . int
       )
  <> range . lit "range" . slash . int . slash . int
  )
