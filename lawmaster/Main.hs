{-# language OverloadedStrings #-}

import Data.List (delete, stripPrefix)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Dredd.IO
import qualified GHC.SourceGen as SourceGen
import qualified System.Environment as Environment
import qualified System.FilePath as FilePath
import qualified System.FilePath.Glob as FilePath
import qualified System.IO as IO

generateTestDriver :: SourceGen.ModuleNameStr -> [SourceGen.ModuleNameStr] -> SourceGen.HsModule'
generateTestDriver moduName tests =
  SourceGen.module'
    (Just moduName)
    (Just [SourceGen.var "main"])
    ([ SourceGen.exposing (SourceGen.import' "Hedgehog.Main") [SourceGen.var "defaultMain"]
     ] <> fmap (SourceGen.qualified' . SourceGen.import') tests
    )
    [ SourceGen.typeSig "main" $ SourceGen.var "IO" SourceGen.@@ SourceGen.var "()",
      SourceGen.valBind "main"
      $ SourceGen.var "defaultMain"
        SourceGen.@@ SourceGen.list
                     ( fmap
                       (SourceGen.var . flip SourceGen.qual "dreddLaws")
                       tests
                     )
    ]

findTestModules :: FilePath -> IO [SourceGen.ModuleNameStr]
findTestModules src =
  fmap
  ( fmap
    ( fromString
      . replacePathSepTo '.'
      . FilePath.dropExtension
      . fromMaybe (error "bad file")
      . stripPrefix (directory <> [FilePath.pathSeparator])
    )
    . delete src
  )
  $ FilePath.globDir1 (FilePath.compile "**/*.hs") directory
  where
    directory = FilePath.takeDirectory src
    replacePathSepTo c1 = fmap $ \c2 -> if FilePath.isPathSeparator c2 then c1 else c2

main :: IO ()
main = do
  args <- Environment.getArgs
  case args of
    src : _ : dst : _opts ->
      IO.withFile
      dst
      IO.WriteMode
      (\h -> writeModule h . generateTestDriver "Main" =<< findTestModules src)
    _ -> error "bad args"
