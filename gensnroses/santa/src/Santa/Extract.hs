{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Santa.Extract (
    extractArbitraries
  , failExtractArbitrariesError
  , filterArbitraryInstances
  , mapArbitraryInstances
  ) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC

import           Santa.Prelude

import qualified System.Directory as Directory
import           System.FilePath (FilePath, (</>))
import qualified System.FilePath as FilePath
import           System.Exit (ExitCode (..))

data ExtractArbitrariesError =
    ExtractFileNotFound FilePath
  | ExtractTarError Tar.FormatError
  deriving (Eq, Show)

extractArbitraries :: FilePath -> FilePath -> ExceptT ExtractArbitrariesError IO ()
extractArbitraries dir file = do
  let
    out = dir </> (FilePath.takeBaseName . FilePath.takeBaseName) file <> ".hs"
  e <- liftIO . Directory.doesFileExist $ out
  unless e $
    extractArbitrariesTo file out

extractArbitrariesTo :: FilePath -> FilePath -> ExceptT ExtractArbitrariesError IO ()
extractArbitrariesTo file out = do
  ex <- liftIO . Directory.doesFileExist $ file
  unless ex $
    throwE . ExtractFileNotFound $ file
  liftIO . Directory.createDirectoryIfMissing True . FilePath.takeDirectory $ out
  let
    extractFile :: Tar.Entry -> Either ExtractArbitrariesError BSL.ByteString -> Either ExtractArbitrariesError BSL.ByteString
    extractFile e a = do
      bs1 <- a
      if ((==) ".hs" . FilePath.takeExtension . Tar.entryPath $ e) then
        case Tar.entryContent e of
          Tar.NormalFile b _ -> do
            bs2 <- pure . filterArbitraryInstances $ b
            let
              -- Filepath starts with the archive name
              comment = "-- " <> (BSLC.pack . Tar.entryPath) e
            pure $ if BSL.null bs2 then bs1 else bs1 <> comment <> "\n" <> bs2
          _ ->
            -- People symlink haskell files sometimes
            -- "unclechu-xmonadrc-f9f83e0/xmonad/src/XMonad/Hooks/SetWMName.hs"
            pure bs1
      else
        pure bs1
  bs <- liftIO $ BSL.readFile file
  bsa <- hoistEither . Tar.foldEntries extractFile (pure "") (Left . ExtractTarError) . Tar.read . GZip.decompress $ bs
  liftIO . BSL.writeFile (out <> ".bak") $ bsa
  liftIO $ Directory.renameFile (out <> ".bak") out

filterArbitraryInstances :: BSL.ByteString -> BSL.ByteString
filterArbitraryInstances =
    BSLC.unlines . fmap BSL.fromStrict . join . mapArbitraryInstances id

mapArbitraryInstances :: ([BS.ByteString] -> a) -> BSL.ByteString -> [a]
mapArbitraryInstances f =
  let
    foldLines bs r =
      case bs of
        [] ->
          foldLines ([] : bs) r
        [] : _ ->
          if BS.isPrefixOf "instance" r && BS.isInfixOf " Arbitrary " r then
            -- Start of the instance
            [r] : bs
          else
            -- Ignore this line
            bs
        h : t ->
          -- Tabs: http://hackage.haskell.org/package/Barracuda-1.0.2/src/Tests/QuickCheck/X509.hs
          if BS.null r || BS.take 1 r == " " || BS.take 1 r == "\t" then
            -- Still in the instance
            (r : h) : t
          else
            -- We've reached the end of the instance, but may be a new one
            foldLines ([] : bs) r
  in
    reverse . fmap (f . reverse) . filter (not . null) . foldl' foldLines [] . fmap BSL.toStrict . BSLC.lines

failExtractArbitrariesError :: ExtractArbitrariesError -> (String, ExitCode)
failExtractArbitrariesError pe =
  case pe of
    ExtractFileNotFound f ->
      ("Could not find haskell file: " <> f, ExitFailure 1)
    ExtractTarError te ->
      ("Error reading tar " <> show te, ExitFailure 1)
