{-# language LambdaCase #-}

-- | What to expose:
-- 1. a function starting from `ModSummary`, so we can use it as a `typeCheckResultAction`,
-- 2. a `main` taking a `String` on the command-line to be interpreted by `guessTarget`, and
-- 3. whatever kind of thing tasty-discover is provided as
module Dredd (plugin) where

import Control.Arrow ((&&&))
import Dredd.Judge
import qualified GHC
import qualified GHC.Paths as Paths
import qualified GHC.SourceGen as SourceGen
import qualified GhcPlugins
import qualified MonadUtils
import qualified Plugins
import qualified TcRnTypes
import qualified System.Directory as Directory
import qualified System.IO as IO

createDreddFile :: GHC.Module -> FilePath
createDreddFile modu =
  "dredd/Judge/Dredd/"
  <> fmap
     (\case
         '.' -> '/'
         x -> x)
     (GhcPlugins.moduleNameString $ GHC.moduleName modu)
  <> ".hs"

-- | To be used as a plugin that extracts the instances during compilation.
outputInstances ::
  [Plugins.CommandLineOption] ->
  GHC.ModSummary ->
  TcRnTypes.TcGblEnv ->
  TcRnTypes.TcM TcRnTypes.TcGblEnv
outputInstances _ _ env =
  let modu = TcRnTypes.tcg_mod env
      moduFile = createDreddFile modu
   in MonadUtils.liftIO $ do
      Directory.createDirectoryIfMissing True moduFile
      Directory.removeDirectory moduFile      
      IO.withFile
        moduFile
        IO.WriteMode
        (\h ->
            uncurry (*>)
            $ (GHC.runGhc (Just Paths.libdir) . SourceGen.hPutPpr h . processInstances modu . TcRnTypes.tcg_insts &&& pure) env
        )

plugin :: Plugins.Plugin
plugin =
  Plugins.defaultPlugin
  {
    Plugins.pluginRecompile = Plugins.purePlugin,
    Plugins.typeCheckResultAction = outputInstances
  }
