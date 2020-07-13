module Dredd.IO
  (writeModule,
  ) where

import qualified GHC
import qualified GHC.Paths as Paths
import qualified GHC.SourceGen as SourceGen
import qualified System.IO as IO

writeModule :: IO.Handle -> SourceGen.HsModule' -> IO ()
writeModule h = GHC.runGhc (Just Paths.libdir) . SourceGen.hPutPpr h
