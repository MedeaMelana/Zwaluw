{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Zwaluw.Regular 
  ( mkRouters
  , Routers
  , RouterList(..)
  
  -- * Re-exported from Generics.Regular
  , deriveAll
  , PF
  ) where

import Web.Zwaluw.Core
import Generics.Regular

infixr :&


-- | The type of the list of routers generated for type @r@.
type Routers r = RouterList (PF r) r

-- | Creates the routers for type @r@, one for each constructor. For example:
--
--   @Z rHome :& Z rUserOverview :& Z rUserDetail :& Z rArticle = mkRouters@
mkRouters :: (MkRouters (PF r), Regular r) => Routers r
mkRouters = mkRouters' to (Just . from)

data family RouterList f r
class MkRouters (f :: * -> *) where
  mkRouters' :: (f r -> r) -> (r -> Maybe (f r)) -> RouterList f r

data instance RouterList (C c f) r = Z (forall t. Router (RouterLhs f r t) (r :- t))
instance MkRouter f => MkRouters (C c f) where
  mkRouters' addLR matchLR = Z $ pure (hdMap (addLR . C) . mkP) (fmap mkS . hdTraverse (fmap unC . matchLR))

data instance RouterList (f :+: g) r = RouterList f r :& RouterList g r
instance (MkRouters f, MkRouters g) => MkRouters (f :+: g) where
  mkRouters' addLR matchLR = mkRouters' (addLR . L) (matchL matchLR) 
                          :& mkRouters' (addLR . R) (matchR matchLR)
    where
      matchL :: (r -> Maybe ((f :+: g) r)) -> r -> Maybe (f r)
      matchL frm r = case frm r of 
        Just (L f) -> Just f
        _ -> Nothing

      matchR :: (r -> Maybe ((f :+: g) r)) -> r -> Maybe (g r)
      matchR frm r = case frm r of 
        Just (R f) -> Just f
        _ -> Nothing


type family RouterLhs (f :: * -> *) (r :: *) (t :: *) :: *
class MkRouter (f :: * -> *) where
  mkP :: RouterLhs f r t -> (f r :- t)
  mkS :: (f r :- t) -> RouterLhs f r t

type instance RouterLhs U r t = t
instance MkRouter U where
  mkP t = U :- t
  mkS (U :- r) = r

type instance RouterLhs (K a) r t = a :- t
instance MkRouter (K a) where
  mkP (a :- t) = K a :- t
  mkS (K a :- t) = a :- t

type instance RouterLhs I r t = r :- t
instance MkRouter I where
  mkP (r :- t) = I r :- t
  mkS (I r :- t) = r :- t

type instance RouterLhs (f :*: g) r t = RouterLhs f r (RouterLhs g r t)
instance (MkRouter f, MkRouter g) => MkRouter (f :*: g) where
  mkP t = (f :*: g) :- t''
    where 
      f :- t'  = mkP t
      g :- t'' = mkP t'
  mkS ((f :*: g) :- t) = mkS (f :- mkS (g :- t))
