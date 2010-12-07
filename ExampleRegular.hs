{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE EmptyDataDecls #-}

import Web.Zwaluw
import Web.Zwaluw.Regular

import Prelude hiding (id, (.), (/))
import Control.Category


-- A datatype modelling all pages in a website.

data Sitemap
   = Home
   | UserOverview
   | UserDetail Int
   | Article Int String
   deriving (Eq, Show)

$(deriveAll ''Sitemap "PF_Sitemap")
type instance PF Sitemap = PF_Sitemap

Z rHome :& Z rUserOverview :& Z rUserDetail :& Z rArticle = mkRouters


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
