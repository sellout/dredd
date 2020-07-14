{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}

module Dredd.Judge where

import Control.Arrow ((***))
import Control.Monad ((<=<), join)
import qualified Data.Char
import Data.Foldable (find)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import qualified GHC
import qualified GHC.SourceGen as SourceGen
import qualified GhcPlugins
import qualified HscTypes
import qualified InstEnv
import qualified Name
import qualified Outputable
import qualified TcType

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

-- | We're often given a module that is "too early" in the process. This
--   converts it to one that has what we need.
processModule :: GHC.GhcMonad m => GHC.ModSummary -> m GHC.ModuleInfo
processModule = fmap GHC.moduleInfo . GHC.typecheckModule <=< GHC.parseModule

-- | Currently, this just turns it into a String, but we should be building a
--   new (source) module here for our test suite.
processInstance :: GHC.ClsInst -> SourceGen.HsExpr'
processInstance = processInstance' . TcType.tcSplitDFunTy . GhcPlugins.idType . InstEnv.is_dfun

-- | The GHC docs on this aren't good, so ... in the example instance definition,
--
-- > instance (Monoid a, Semigroup b) => MyClass (Foo a) (Bar b)
--
--   the arguments to this function would look a bit like
--
-- > ([a, b], [Monoid a, Semigroup b], MyClass, [Foo a, Bar b])
processInstance' :: ([GHC.TyVar], [GHC.Type], GHC.Class, [GHC.Type]) -> SourceGen.HsExpr'
processInstance' = \case
  ([], [], cls, tys) ->
    let tyNames = intercalate ", " $ Outputable.showSDocUnsafe . Outputable.ppr <$> tys
        tyComponentNames = fromMaybe (error "invalid type") . typeComponentNames <$> tys
        lawsName = (<> "Laws") . fnHead Data.Char.toLower $ Name.getOccString cls
        genNames = fmap (fmap ("gen" <>)) tyComponentNames
    in SourceGen.tuple
       [ SourceGen.string tyNames,
         SourceGen.list
         [ foldl
           (\x -> (x SourceGen.@@) . foldr1 (SourceGen.@@) . fmap (SourceGen.var . fromString))
           (SourceGen.var $ fromString lawsName :: SourceGen.HsExpr')
           genNames
         ]
       ]
  (_, [], _, _) -> error "class has type vars"
  ([], _, _, _) -> error "class has constraints"
  (_, _, _, _) -> error "class has multiple bad things"

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
        SourceGen.exposing (SourceGen.import' "Data.Sort") [SourceGen.var "monoidSortAssocs"],
        SourceGen.import' . fromString $ moduName <> ".Gen"
      ]
      [ SourceGen.typeSig lawFunctionName
        $ SourceGen.var "IO" SourceGen.@@ SourceGen.var "Bool",
        SourceGen.valBind lawFunctionName
        $ SourceGen.op
          (SourceGen.var "lawsCheckMany")
          "$"
          ( SourceGen.var "monoidSortAssocs"
            SourceGen.@@ SourceGen.list (fmap processInstance insts)
          )
      ]

fnHead :: (a -> a) -> [a] -> [a]
fnHead f = \case
  [] -> []
  (h : t) -> f h : t
