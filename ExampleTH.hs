{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonoPatBinds #-}

import Web.Zwaluw
import Web.Zwaluw.TH

import Prelude hiding (id, (.), (/))
import Control.Category


-- A datatype modelling all pages in a website.

data Sitemap
   = Home
   | CatOverview
   | CatDetail Int
   | Product Int String
   deriving (Eq, Show)

(rHome, rCatOverview, rCatDetail, rProduct) = $(deriveRouterTuple ''Sitemap)


-- The router. Specifies how to parse a URL into a Sitemap and back.

sitemap :: Router r (Sitemap :- r)
sitemap = id /
    (  rHome
    <> "categories" . cats
    <> rProduct . ("product" / int . "-" . part)
    )
  where
    cats =  rCatOverview
         <> rCatDetail / int
