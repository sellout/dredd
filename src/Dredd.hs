{-# language LambdaCase #-}

module Dredd (plugin) where

import Control.Arrow ((&&&))
import Dredd.IO
import Dredd.Judge
import qualified GHC
import qualified GhcPlugins
import qualified MonadUtils
import qualified Plugins
import qualified TcRnTypes
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
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
      Directory.createDirectoryIfMissing True $ FilePath.takeDirectory moduFile
      IO.withFile
        moduFile
        IO.WriteMode
        (\h ->
            uncurry (*>)
            $ (writeModule h . processInstances modu . TcRnTypes.tcg_insts &&& pure) env
        )

plugin :: Plugins.Plugin
plugin =
  Plugins.defaultPlugin
  {
    Plugins.pluginRecompile = Plugins.purePlugin,
    Plugins.typeCheckResultAction = outputInstances
  }
