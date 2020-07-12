{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}

-- | What to expose:
-- 1. a function starting from `ModSummary`, so we can use it as a `typeCheckResultAction`,
-- 2. a `main` taking a `String` on the command-line to be interpreted by `guessTarget`, and
-- 3. whatever kind of thing tasty-discover is provided as
module Dredd.Judge where

import Control.Arrow ((***))
import Control.Monad ((<=<), join)
import qualified Data.Char
import Data.Foldable (find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import qualified GHC
import qualified GHC.SourceGen as SourceGen
import qualified GhcPlugins
import qualified HscTypes
import qualified InstEnv
import qualified Name
import qualified Outputable

loadModuleSummary ::
  GHC.GhcMonad m =>
  -- | the "filename" of the module we want to load
  String ->
  m (Maybe GHC.ModSummary)
loadModuleSummary modName = do
   -- First, set the target to the desired filename
   target <- GHC.guessTarget modName Nothing
   GHC.addTarget target
   _ <- GHC.load GHC.LoadAllTargets
   -- Then find dependencies
   modGraph <- GHC.depanal [] True
   pure . find ((== modName) . HscTypes.msHsFilePath) $ GHC.mgModSummaries modGraph

-- parseHaskell :: FilePath -> IO (Maybe (HsModule RdrName))
-- parseHaskell file = do
--     initStaticOpts
--     sbuf <- hGetStringBuffer file
--     let srcloc = mkSrcLoc (mkFastString file) 1 1
--     return $ case unP parseModule (mkPState defaultDynFlags sbuf srcloc) of
--         POk _ (L _ mdl) -> Just mdl
--         PFailed _ _     -> Nothing

-- | We're often given a module that is "too early" in the process. This
--   converts it to one that has what we need.
processModule :: GHC.GhcMonad m => GHC.ModSummary -> m GHC.ModuleInfo
processModule = fmap GHC.moduleInfo . GHC.typecheckModule <=< GHC.parseModule

-- | Currently, this just turns it into a String, but we should be building a
--   new (source) module here for our test suite.
processInstance :: GHC.ClsInst -> SourceGen.HsExpr'
processInstance ci = do
  let (_, cls, [ty]) = InstEnv.instanceHead ci
      tyName = Outputable.showSDocUnsafe $ Outputable.ppr ty
      tyComponentNames = fromMaybe (error "invalid type") $ typeComponentNames ty
      lawsName = (<> "Laws") . fnHead Data.Char.toLower $ Name.getOccString cls
      genNames = fmap ("gen" <>) tyComponentNames
   in SourceGen.tuple
      [ SourceGen.string tyName,
        SourceGen.list
        [ foldr1 (SourceGen.@@) . fmap (SourceGen.var . fromString)
          $ lawsName :| genNames
        ]
      ]

typeComponentNames :: GhcPlugins.Type -> Maybe [String]
typeComponentNames =
  uncurry (fmap . (:))
  . (Name.getOccString *** fmap join . traverse typeComponentNames)
  <=< GhcPlugins.splitTyConApp_maybe

lawFunctionName :: SourceGen.OccNameStr
lawFunctionName = "dreddLaws"

processInstances :: GHC.Module -> [GHC.ClsInst] -> SourceGen.HsModule'
processInstances modu insts =
  let moduName = GhcPlugins.moduleNameString $ GHC.moduleName modu
   in SourceGen.module'
      (Just . fromString $ "Judge.Dredd." <> moduName)
      (Just [SourceGen.var "dreddLaws"])
      [ SourceGen.import' "Hedgehog.Classes",
        SourceGen.import' . fromString $ moduName <> ".Gen"
      ]
      [ SourceGen.typeSig lawFunctionName . SourceGen.listTy
        $ SourceGen.tuple
          [SourceGen.var "String", SourceGen.listTy $ SourceGen.var "Laws"],
        SourceGen.valBind lawFunctionName . SourceGen.list
        $ fmap processInstance insts
      ]

fnHead :: (a -> a) -> [a] -> [a]
fnHead f = \case
  [] -> []
  (h : t) -> f h : t
