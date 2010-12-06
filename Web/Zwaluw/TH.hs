{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Zwaluw.TH where

import Web.Zwaluw.Core
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Monad


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
    NormalC name tys -> go name (map snd tys)
    RecC name tys -> go name (map (\(_,_,ty) -> ty) tys)
    _ -> do
      runIO $ putStrLn $ "Skipping unsupported constructor " ++ show (conName con)
      return []
  where
    go name tys = do
      let name' = mkRouterName name
      runIO $ putStrLn $ "Introducing router " ++ nameBase name' ++ "."
      exp <- [| pure $(deriveConstructor name (length tys))
                     $(deriveDestructor name tys) |]
      return [FunD name' [Clause [] (NormalB exp) []]]


-- Derive the contructor part of a router.
deriveConstructor :: Name -> Int -> Q Exp
deriveConstructor name arity = [| $(mk arity) $(conE name) |]
  where
    mk :: Int -> ExpQ
    mk 0 = [| (:-) |]
    mk n = [| arg $(mk (n - 1)) |]


-- Derive the destructor part of a router.
deriveDestructor :: Name -> [Type] -> Q Exp
deriveDestructor name tys = do
  -- Introduce some names
  x          <- newName "x"
  r          <- newName "r"
  fieldNames <- replicateM (length tys) (newName "a")

  -- Figure out the names of some constructors
  nothing    <- [| Nothing |]
  ConE just  <- [| Just |]
  ConE cons  <- [| (:-) |]

  let conPat   = ConP name (map VarP fieldNames)
  let okBody   = ConE just `AppE`
                  foldr
                    (\h t -> ConE cons `AppE` VarE h `AppE` t)
                    (VarE r)
                    fieldNames
  let okCase   = Match (ConP cons [conPat, VarP r]) (NormalB okBody) []
  let failCase = Match WildP (NormalB nothing) []

  return $ LamE [VarP x] (CaseE (VarE x) [okCase, failCase])


-- Derive the name of a router based on the name of the constructor in question.
mkRouterName :: Name -> Name
mkRouterName name = mkName ('r' : nameBase name)


-- Retrieve the name of a constructor.
conName :: Con -> Name
conName con =
  case con of
    NormalC name _  -> name
    RecC name _     -> name
    InfixC _ name _ -> name
    ForallC _ _ con -> conName con