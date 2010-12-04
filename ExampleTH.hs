{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Web.Zwaluw
import Web.Zwaluw.TH

import Prelude hiding (id, (.), (/))
import Control.Category


-- A datatype modelling all pages in a website.

data Sitemap
   = Home
   | UserOverview
   | UserDetail Int
   | Article Int String
   deriving (Eq, Show)

$(deriveRouters ''Sitemap)


-- The router. Specifies how to parse a URL into a Sitemap and back.

sitemap :: Router r (Sitemap :- r)
sitemap = id /
    (  rHome
    <> "users" . users
    <> rArticle . ("article" / int . "-" . part)
    )
  where
    users  = rUserOverview
          <> rUserDetail / int
