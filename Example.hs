{-# LANGUAGE TypeOperators #-}

import Web.Zwaluw
import Prelude hiding (id, (.))
import Control.Category


data Sitemap
   = Home
   | Range Int Int
   | CaseOverview
   | CaseDetail Int
   deriving (Eq, Show)

home :: Router r (Sitemap :- r)
home = constr0 Home $ \a -> do Home <- a; return ()

caseOverview :: Router r (Sitemap :- r)
caseOverview = constr0 CaseOverview $ \a -> do CaseOverview <- a; return ()

caseDetail :: Router (Int :- r) (Sitemap :- r)
caseDetail = constr1 CaseDetail $ \a -> do CaseDetail i <- a; return i

range :: Router (Int :- Int :- r) (Sitemap :- r)
range = constr2 Range $ \a -> do Range l u <- a; return (l, u)

url :: Router r (Sitemap :- r)
url = 
  slash . 
  (  home . lit "home"
  <> lit "cases" .
       (  caseOverview
       <> caseDetail . slash . int
       )
  <> range . lit "range" . slash . int . slash . int
  )
