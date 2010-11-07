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

home :: Constr0 Sitemap
home = constr0 Home $ \a -> do Home <- Just a; Just ()

caseOverview :: Constr0 Sitemap
caseOverview = constr0 CaseOverview $ \a -> do CaseOverview <- Just a; Just ()

caseDetail :: Constr1 Sitemap Int
caseDetail = constr1 CaseDetail $ \a -> do CaseDetail i <- Just a; Just i

range :: Constr2 Sitemap Int Int
range = constr2 Range $ \a -> do Range l u <- Just a; Just (l, u)

url :: P t (Sitemap :- t)
url = 
  slash . 
  (  home . lit "home"
  <> lit "cases" .
       (  caseOverview
       <> caseDetail . slash . int
       )
  <> range . lit "range" . slash . int . slash . int
  )
