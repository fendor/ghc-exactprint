{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
-- We parse a module and rearrange the functions depending on their order in the import list.


import GHC.Paths

import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.Syntax.Decls
import Language.Haskell.Syntax
import GHC.Hs.Extension
import GHC.Hs.ImpExp
import GHC.Types.Name.Reader
import GHC.Types.SrcLoc

import Data.Maybe
import qualified Data.Map as Map

main :: IO ()
main = do
  Right (L l hsmod) <- parseModule libdir "examples/imports.hs"
  let decls = makeDeltaAst $ hsmodDecls hsmod
  let
      newDecls =
        case hsmodExports hsmod  of
            Nothing -> decls
            Just exports ->
              let
                  cares = map showGhc $ mapMaybe (sortExports . unLoc) (unLoc exports)
                  mkKey v = maybe "default" showGhc (getKey . unLoc $ v)
                  declMap = foldr (\v -> Map.insertWith (++) (mkKey v) [v]) Map.empty decls
                  (sorted, leftover)
                    = foldr (\k (r, m) -> case Map.lookup k m of
                                      Nothing -> (r,m)
                                      Just vs -> (vs ++ r, Map.delete k m)) ([], declMap) cares
              in sorted ++ concatMap snd (Map.toAscList leftover)
  -- We do not need to change the annotations at all
  putStrLn $ exactPrint (L l hsmod { hsmodDecls = newDecls })


-- | Find the names of things we might care about
sortExports :: IE GhcPs -> Maybe RdrName
sortExports (IEVar _ n _)  = Just (lieWrappedName n)
sortExports (IEThingAbs _ n _) = Just (lieWrappedName n)
sortExports (IEThingAll _ n _) = Just (lieWrappedName n)
sortExports (IEThingWith _ n _ _ _) = Just (lieWrappedName n)
sortExports (IEModuleContents _ _) = Nothing
-- Haddock constructors
sortExports _ = Nothing

-- Get the key to sort decls by
getKey :: HsDecl GhcPs -> Maybe RdrName
--getKey (TyClDecl t) = Just (unLoc $ tyClDeclLname  t)
getKey (SigD _ t) =
  case t of
       TypeSig _ (n:_) _ -> Just (unLoc $ n)
       PatSynSig _ (n:_) _ -> Just (unLoc $ n)
       _ -> Nothing
getKey (ValD _ t) =
  case t of
       FunBind{fun_id} -> Just (unLoc $ fun_id)
       PatSynBind _ b    -> Just (unLoc $ psb_id b)
       -- Can't appear at the top level
       PatBind{} -> Nothing
       -- Renamed
       _ -> Nothing
getKey _ = Nothing
