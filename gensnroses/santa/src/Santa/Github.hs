{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Santa.Github (
    Repository (..)
  ---
  , listRepositories
  , failListRepositoriesError
  ---
  , getRepository
  , failGetRepositoryError
  ) where

import           Data.Aeson.Types (Parser, Value, (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as BSL
import           Network.HTTP.Client (Response)
import qualified Network.HTTP.Client as HC
import           Network.HTTP.Types (Status (..))

import           Santa.Prelude
import           Santa.Http

import qualified System.Directory as Directory
import           System.FilePath (FilePath)
import qualified System.FilePath as FilePath
import           System.Exit (ExitCode (..))

data Repository =
  Repository {
      repositoryOwner :: String
    , repositoryName :: String
    } deriving (Eq, Show)

-------

data ListRepositoriesError =
    ListRepositoriesInvalidResponse (Response BSL.ByteString)
  | ListRepositoriesInvalidJson String
  deriving (Eq, Show)

listRepositories :: HC.Manager -> Int -> (Repository -> IO a) -> ExceptT ListRepositoriesError IO [a]
listRepositories mgr max' with' =
  let
    go l p =
      if p >= max' then
        pure l
      else do
        rs <- listRepositoriesAt mgr p
        l' <- fmap (l <>) . liftIO . mapM with' $ rs
        if null rs then
          pure l'
        else
          go l' (p + 1)
  in
    go [] 0

listRepositoriesAt :: HC.Manager -> Int -> ExceptT ListRepositoriesError IO [Repository]
listRepositoriesAt mgr page = do
  resp <- liftIO . httpGet mgr $
    "https://api.github.com/search/repositories?sort=updated&order=desc&q=language:Haskell&page=" <> show page
  b <- case HC.responseStatus resp of
    Status 200 _ ->
      pure $ HC.responseBody resp
    _ ->
      throwE $ ListRepositoriesInvalidResponse resp
  hoistEither . first ListRepositoriesInvalidJson . (=<<) (Aeson.parseEither parseListRepositories) . Aeson.eitherDecode $ b

parseListRepositories :: Value -> Parser [Repository]
parseListRepositories =
  Aeson.withObject "root" $ \o -> do
    items <- o .: "items"
    for items $ \i ->
       Repository
         <$> (i .: "owner" >>= \o' -> o' .: "login")
         <*> i .: "name"

failListRepositoriesError :: ListRepositoriesError -> (String, ExitCode)
failListRepositoriesError pe =
  case pe of
    ListRepositoriesInvalidResponse r ->
      ("Invalid request to github repository search:\n" <> show r, ExitFailure 1)
    ListRepositoriesInvalidJson r ->
      ("Invalid response from github repository search: " <> r, ExitFailure 1)

-------

data GetRepositoryError =
    GetRepositoryInvalidResponse (Response BSL.ByteString)
  | GetRepositoryInvalidJson String
  deriving (Eq, Show)

getRepository :: HC.Manager -> FilePath -> Repository -> ExceptT GetRepositoryError IO ()
getRepository mgr dest (Repository owner repo) = do
  liftIO . Directory.createDirectoryIfMissing True . FilePath.takeDirectory $ dest
  resp <- liftIO . httpGet mgr $
    "https://api.github.com/repos/" <> owner <> "/" <> repo <> "/tarball"
  b <- case HC.responseStatus resp of
    Status 200 _ ->
      pure $ HC.responseBody resp
    _ ->
      throwE $ GetRepositoryInvalidResponse resp
  liftIO $
    BSL.writeFile dest b

failGetRepositoryError :: GetRepositoryError -> (String, ExitCode)
failGetRepositoryError pe =
  case pe of
    GetRepositoryInvalidResponse r ->
      ("Invalid request to github repository search:\n" <> show r, ExitFailure 1)
    GetRepositoryInvalidJson r ->
      ("Invalid response from github repository search: " <> r, ExitFailure 1)
