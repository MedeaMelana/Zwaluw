{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (id, (.), (/))
import Control.Category
import Web.Zwaluw


-- A datatype modelling all pages in a website.

data Sitemap
   = Home
   | UserOverview
   | UserDetail Int
   | Range Int Int
   deriving (Eq, Show)


-- Constructor routers. Soon to be generated by Template Haskell.

home :: Router r (Sitemap :- r)
home = constr0 Home $ \a -> do Home <- a; return ()

userOverview :: Router r (Sitemap :- r)
userOverview = constr0 UserOverview $ \a -> do UserOverview <- a; return ()

userDetail :: Router (Int :- r) (Sitemap :- r)
userDetail = constr1 UserDetail $ \a -> do UserDetail i <- a; return i

range :: Router (Int :- Int :- r) (Sitemap :- r)
range = constr2 Range $ \a -> do Range l u <- a; return (l, u)


-- The router. Specifies how to parse a URL into a Sitemap and back.

sitemap :: Router r (Sitemap :- r)
sitemap = id /
  (  home
  <> "users" . (userOverview <> userDetail / int)
  <> "range" . range / int / int
  )
