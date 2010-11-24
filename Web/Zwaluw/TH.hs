{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Zwaluw.TH where

import Web.Zwaluw
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Monad

arg :: (ty -> r -> s) -> (a -> ty) -> (a :- r) -> s
arg c f (x :- r) = c (f x) r

-- TyConI
--   (DataD [] Main.Sitemap []
--     [NormalC Main.Home []
--     ,NormalC Main.UserOverview []
--     ,NormalC Main.UserDetail [(NotStrict,ConT GHC.Types.Int)]
--     ,NormalC Main.Article [(NotStrict,ConT GHC.Types.Int),(NotStrict,ConT GHC.Base.String)]
--     ] [])

-- TyConI (NewtypeD [] Main.WrapSitemap [] (NormalC Main.WrapSitemap [(NotStrict,ConT Main.Sitemap)]) [])

-- Derive routers for all constructors in a datatype.
deriveRouters :: Name -> Q [Dec]
deriveRouters name = do
  info <- reify name
  case info of
    TyConI (DataD _ _ _ cons _)   ->
      concat `liftM` mapM deriveRouter cons
    TyConI (NewtypeD _ _ _ con _) ->
      deriveRouter con
    _ ->
      fail $ show name ++ " is not a datatype."

-- Derive a router for a single constructor.
deriveRouter :: Con -> Q [Dec]
deriveRouter con =
  case con of
    NormalC name tys -> do
      exp <- [| pure $(deriveConstructor name (length tys)) $(deriveDestructor con) |]
      return [FunD (mkRouterName name) [Clause [] (NormalB exp) []]]
    -- RecC conName tys -> return []
    _ -> do
      runIO $ putStrLn $ "Skipping unsupported constructor " ++ show (conName con)
      return []

-- Derive the contructor part of a router.
deriveConstructor :: Name -> Int -> Q Exp
deriveConstructor name arity = [| $(mk arity) $(conE name) |]
  where
    mk :: Int -> ExpQ
    mk 0 = [| (:-) |]
    mk n = [| arg $(mk (n - 1)) |]

-- Derive the destructor part of a router.
deriveDestructor :: Con -> Q Exp
deriveDestructor con = [| undefined |]

-- Derive the name of a router based on the name of the constructor in question.
mkRouterName :: Name -> Name
mkRouterName name = mkName ("r" ++ nameBase name)

-- Retrieve the name of a constructor.
conName :: Con -> Name
conName con =
  case con of
    NormalC name _  -> name
    RecC name _     -> name
    InfixC _ name _ -> name
    ForallC _ _ con -> conName con
