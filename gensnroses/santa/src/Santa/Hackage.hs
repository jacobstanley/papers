{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Santa.Hackage (
    renderPackage
  ---
  , getLatestPackages
  , failGetPackagesError
  ---
  , DownloadPackageResult (..)
  , downloadPackage
  , failDownloadPackageError
  ) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as M
import           Data.Version (Version)
import qualified Data.Version as Version

import           Network.HTTP.Client (Response)
import qualified Network.HTTP.Client as HC
import           Network.HTTP.Types (Status (..))

import           Santa.Prelude
import           Santa.Http

import qualified System.Directory as Directory
import           System.FilePath (FilePath, (</>))
import qualified System.FilePath as FilePath
import           System.Exit (ExitCode (..))

import           Text.ParserCombinators.ReadP (readP_to_S)

data Package =
  Package String Version
  deriving (Eq, Show)

renderPackage :: Package -> String
renderPackage (Package p v) =
  p <> "-" <> Version.showVersion v

-------

data GetPackagesError =
    PackagesFileNotFound (Response BSL.ByteString)
  | PackagesTarError Tar.FormatError
  | PackagesInvalidFile FilePath
  | PackagesInvalidVersion String
  deriving (Eq, Show)

getLatestPackages :: HC.Manager -> ExceptT GetPackagesError IO [Package]
getLatestPackages mgr = do
  resp <- liftIO . httpGet mgr $
    "http://hackage.haskell.org/packages/index.tar.gz"
  b <- case HC.responseStatus resp of
    Status 200 _ ->
      pure $ HC.responseBody resp
    _ ->
      throwE $ PackagesFileNotFound resp

  hoistEither . entriesToPackages . Tar.read . GZip.decompress $ b

entriesToPackages :: Tar.Entries Tar.FormatError -> Either GetPackagesError [Package]
entriesToPackages =
  let
    foldEntry e ps = do
      p <- entryToPackage e
      pure $ case p of
        Nothing ->
          ps
        Just (Package n v) ->
          M.insertWith max n v ps
    entryToPackage e =
      case FilePath.splitDirectories . Tar.entryPath $ e of
        -- Ignore these preferred version files, it shouldn't matter to the test files
        _ : "preferred-versions" : [] ->
          pure Nothing
        n : v : _ : [] ->
          case reverse . readP_to_S Version.parseVersion $ v of
            [] ->
              Left . PackagesInvalidVersion $ v
            (v', _) : _ ->
              pure . pure $ Package n v'
        _ ->
          Left . PackagesInvalidFile . Tar.entryPath $ e
  in
    fmap (fmap (uncurry Package) . M.toList) .
      Tar.foldEntries (\e ps -> ps >>= foldEntry e) (pure mempty) (Left . PackagesTarError)

failGetPackagesError :: GetPackagesError -> (String, ExitCode)
failGetPackagesError pe =
  case pe of
    PackagesFileNotFound r ->
      ("Could not find package index file:\n" <> show r, ExitFailure 1)
    PackagesTarError te ->
      ("Error reading tar " <> show te, ExitFailure 1)
    PackagesInvalidFile f ->
      ("Error parsing package file path: " <> f, ExitFailure 1)
    PackagesInvalidVersion v ->
      ("Error parsing version: " <> v, ExitFailure 1)

-------

data DownloadPackageError =
    PackageFileNotFound (HC.Response BSL.ByteString)
  deriving (Eq, Show)

data DownloadPackageResult =
    DownloadPackageSuccess
  | DownloadPackageFail

downloadPackage :: HC.Manager -> FilePath -> Package -> ExceptT DownloadPackageError IO (DownloadPackageResult, FilePath)
downloadPackage mgr dir pkg = do
  let
    file = dir </> renderPackage pkg <> ".tar.gz"
  e <- liftIO . Directory.doesFileExist $ file
  result <- if e then
      pure DownloadPackageSuccess
    else
      downloadPackageForce mgr dir pkg
  pure (result, file)

downloadPackageForce :: HC.Manager -> FilePath -> Package -> ExceptT DownloadPackageError IO DownloadPackageResult
downloadPackageForce mgr dir pkg = do
  resp <- liftIO . httpGet mgr $
    "http://hackage.haskell.org/package/" <> renderPackage pkg <> "/" <> renderPackage pkg <> ".tar.gz"
  case HC.responseStatus resp of
    Status 200 _ -> do
      liftIO $ do
        Directory.createDirectoryIfMissing True dir
        let
          path = dir </> renderPackage pkg <> ".tar.gz"
        BSL.writeFile (path <> ".bak") . HC.responseBody $ resp
        Directory.renameFile (path <> ".bak") path
        pure DownloadPackageSuccess
    Status 500 _ ->
      -- http://hackage.haskell.org/package/hermes
      pure DownloadPackageFail
    _ ->
      throwE $ PackageFileNotFound resp

failDownloadPackageError :: DownloadPackageError -> (String, ExitCode)
failDownloadPackageError pe =
  case pe of
    PackageFileNotFound r ->
      ("Could not find package:\n" <> show r, ExitFailure 1)
