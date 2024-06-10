{-# LANGUAGE NamedFieldPuns #-}

import GHC.Paths

import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Parsers
import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Utils

import qualified Data.Map as Map

import GHC hiding (parseModule)

main :: IO ()
main = do
  Right m <- parseModule libdir "examples/test.hs"
  finalM <- addSignature "baz" "baz :: String -> Int" $ makeDeltaAst m
  -- (finalAs, finalM) <- addSignature2 "baz" "baz :: String -> Int" as mmakeDeltaAstmakeDeltaAst
  putStrLn $ exactPrint finalM


addSignature :: String -- ^ Function to add a signature for
             -> String -- ^ Type signature
             -> ParsedSource
             -> IO ParsedSource
addSignature funid tsig (GHC.L l m) = do
  Right sig <- withDynFlags libdir (\d -> parseDecl d "template" tsig)

  let (before, (bind: after)) = break findFunBind (GHC.hsmodDecls m)
      finalMod = m { GHC.hsmodDecls = before ++ insertAtStart bind sig ++ after }

  return (GHC.L l finalMod)
  where
    findFunBind :: GHC.LHsDecl GHC.GhcPs -> Bool
    findFunBind (GHC.L _ (GHC.ValD _ b@(GHC.FunBind {})))
      | showGhc (GHC.unLoc (GHC.fun_id  b)) == funid = True
    findFunBind _ = False

-- ---------------------------------------------------------------------

-- addSignature2 :: String -- ^ Function to add a signature for
--              -> String -- ^ Type signature
--              -> Module
--              -> IO Module
-- addSignature2 funid tsig m = do
--   Right (sig@(GHC.L ls (GHC.SigD _ s))) <- withDynFlags libdir (\d -> parseDecl d "template" tsig)
--   -- let sigAnns' = setPrecedingLines (GHC.L ls s) 1 0 sigAnns

--   let
--       doAddSig = do
--          tlDecs <- hsDecls m
--          let (before, (bind: after)) = break findFunBind tlDecs

--          modifyAnnsT (setPrecedingLines sig 1 0)
--          modifyAnnsT (setPrecedingLines bind 1 0)

--          replaceDecls m (before ++ [sig,bind] ++ after)

--   let (lp',(ans',_),_w) = runTransform doAddSig
--   return lp'

--   where
--     findFunBind :: GHC.LHsDecl GHC.GhcPs -> Bool
--     findFunBind (GHC.L _ (GHC.ValD _ b@(GHC.FunBind {})))
--       | showGhc (GHC.unLoc (GHC.fun_id  b)) == funid = True
--     findFunBind _ = False
