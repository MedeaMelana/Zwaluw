{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Zwaluw.Regular (mkRouters, Routers, RouterList(..), RouterLhs, deriveAll, PF) where

import Web.Zwaluw.Core
import Generics.Regular

infixr :&


type Routers r = RouterList (PF r) r

mkRouters :: (f ~ PF r, MkRouters f r, Regular r) => Routers r
mkRouters = mkRouters' to (Just . from)

class MkRouters (f :: * -> *) (r :: *) where
  data RouterList f r
  mkRouters' :: (f r -> r) -> (r -> Maybe (f r)) -> RouterList f r

instance MkRouter f r => MkRouters (C c f) r where
  data RouterList (C c f) r = Z (forall t. Router (RouterLhs f r t) (r :- t))
  mkRouters' addLR matchLR = Z $ pure (hdMap (addLR . C) . mkP) (fmap mkS . hdTraverse (fmap unC . matchLR))

instance (MkRouters f r, MkRouters g r) => MkRouters (f :+: g) r where
  data RouterList (f :+: g) r = RouterList f r :& RouterList g r
  mkRouters' addLR matchLR = mkRouters' (addLR . L) (matchL matchLR) 
                          :& mkRouters' (addLR . R) (matchR matchLR)
    where
      matchL :: (r -> Maybe ((f :+: g) r)) -> r -> Maybe (f r)
      matchL frm r = case (frm r) of 
        Just (L f) -> Just f
        _ -> Nothing

      matchR :: (r -> Maybe ((f :+: g) r)) -> r -> Maybe (g r)
      matchR frm r = case (frm r) of 
        Just (R f) -> Just f
        _ -> Nothing


type family RouterLhs (f :: * -> *) (r :: *) (t :: *) :: *
class MkRouter (f :: * -> *) (r :: *) where
  mkP :: RouterLhs f r t -> (f r :- t)
  mkS :: (f r :- t) -> RouterLhs f r t

type instance RouterLhs U r t = t
instance MkRouter U r where
  mkP t = U :- t
  mkS (U :- r) = r

type instance RouterLhs (K a) r t = a :- t
instance MkRouter (K a) r where
  mkP (a :- t) = K a :- t
  mkS (K a :- t) = a :- t

type instance RouterLhs I r t = r :- t
instance MkRouter I r where
  mkP (r :- t) = I r :- t
  mkS (I r :- t) = r :- t

type instance RouterLhs (f :*: g) r t = RouterLhs f r (RouterLhs g r t)
instance (MkRouter f r, MkRouter g r) => MkRouter (f :*: g) r where
  mkP t = (f :*: g) :- t''
    where 
      f :- t'  = mkP t
      g :- t'' = mkP t'
  mkS ((f :*: g) :- t) = mkS (f :- mkS (g :- t))
