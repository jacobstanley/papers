{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Client.OpenSSL as OpenSSL

import qualified OpenSSL.Session as OpenSSL

import           Santa.Extract
import           Santa.Github
import           Santa.Hackage
import           Santa.Prelude

import qualified System.Directory as Directory
import           System.FilePath (FilePath, (</>))
import qualified System.Environment as Env
import           System.Exit (ExitCode (..), exitWith)
import qualified System.IO as IO

main :: IO ()
main = do
  args <- Env.getArgs
  let
    cache = "cache"
    dirArb = cache </> "arbitraries"
    dirArbGh = cache </> "arbitraries_gh"
  case args of
    "github" : [] -> do
      github cache dirArbGh
      stats dirArbGh
    "hackage" : [] ->
      hackage cache dirArb
    "stats" : [] ->
      stats dirArb
    _ -> do
      hackage cache dirArb
      stats dirArb

hackage :: FilePath -> FilePath -> IO ()
hackage cache dirArb = do
  mgr <- HC.newManager HC.defaultManagerSettings
  packages <- orDie failGetPackagesError $ getLatestPackages mgr
  let
    dirTar = cache </> "source"
  for_ packages $ \pkg -> do
    IO.putStrLn $ "Downloading " <> renderPackage pkg
    (result, file) <- orDie failDownloadPackageError $ downloadPackage mgr dirTar pkg
    case result of
      DownloadPackageFail ->
        IO.hPutStrLn IO.stderr $ "Could not download package " <> renderPackage pkg
      DownloadPackageSuccess ->
        orDie failExtractArbitrariesError $ extractArbitraries dirArb file

stats :: FilePath -> IO ()
stats dir = do
  fs <- filter (not . flip elem [".", ".."]) <$> Directory.getDirectoryContents dir
  is <- fmap join . for fs $ \f -> do
    b <- BSL.fromStrict <$> BS.readFile (dir </> f)
    pure . flip mapArbitraryInstances b $ \bs ->
      any (BS.isInfixOf " shrink ") bs
  IO.putStrLn $ "Arbitraries: " <> (show . length) is
  IO.putStrLn $ "Shrinks: " <> (show . length . filter id) is

github :: FilePath -> FilePath -> IO ()
github cache dirArb =
  OpenSSL.withOpenSSL $ do
    mgr <- HC.newManager . OpenSSL.opensslManagerSettings $ OpenSSL.context
    void . orDie failListRepositoriesError . listRepositories mgr 100 $ \r -> do
      IO.putStrLn $ "[github] Downloading " <> repositoryOwner r <> "/" <> repositoryName r
      let
        file = cache </> "github" </> repositoryOwner r <> "_" <> repositoryName r <> ".tar.gz"
      e <- Directory.doesFileExist file
      unless e $
        orDie failGetRepositoryError $ getRepository mgr file r
      er <- runExceptT $ extractArbitraries dirArb file
      case er of
        Left x ->
          -- FIX Failing on crowley100/distroFS
          IO.print x
        Right () ->
          pure ()

-------

orDie :: (e -> (String, ExitCode)) -> ExceptT e IO a -> IO a
orDie f (ExceptT m) = do
  e' <- m
  case e' of
    Right a ->
      pure a
    Left e -> do
      let
        (s, c) = f e
      IO.hPutStrLn IO.stderr s
      exitWith c

