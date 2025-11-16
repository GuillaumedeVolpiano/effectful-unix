{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE UnliftedDatatypes   #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Effectful.Unix
  (Unix
  , runUnix
  , createLink
  , fileID
  , fileIDRaw
  , fileSize
  , getDirectoryContents
  , isDirectory
  , listDirectory
  , removeLink
  , FileID
  , FileOffset)
where
import           Data.Hashable                    (Hashable (hashWithSalt))
import           Effectful                        (Eff, Effect, IOE, liftIO,
                                                   (:>))
import           Effectful.Dispatch.Dynamic       (interpret)
import           Effectful.TH                     (makeEffect)
import qualified System.Directory                 as D (getDirectoryContents,
                                                        listDirectory)
import qualified System.Posix.ByteString          as B (getFileStatus)
import           System.Posix.ByteString.FilePath (RawFilePath)
import qualified System.Posix.Files               as P (createLink, fileID,
                                                        fileSize, isDirectory,
                                                        removeLink)
import           System.Posix.Files               (getFileStatus)
import           System.Posix.Types               (CIno (CIno),
                                                   FileOffset, FileID)

data Unix :: Effect where
  CreateLink :: FilePath -> FilePath -> Unix m ()
  FileID :: FilePath -> Unix m FileID
  FileIDRaw :: RawFilePath -> Unix m FileID
  FileSize :: FilePath -> Unix m FileOffset
  GetDirectoryContents :: FilePath -> Unix m [FilePath]
  IsDirectory :: FilePath -> Unix m Bool
  ListDirectory :: FilePath -> Unix m [FilePath]
  RemoveLink :: FilePath -> Unix m ()

makeEffect ''Unix

instance Hashable CIno where
  hashWithSalt v (CIno x) = hashWithSalt v x

runUnix :: IOE :> es => Eff (Unix : es) a -> Eff es a
runUnix = interpret $ \_ -> \case
                                CreateLink path1 path2 -> liftIO $ P.createLink path1 path2
                                FileID path -> liftIO $ P.fileID <$> getFileStatus path
                                FileIDRaw rawPath -> liftIO $ P.fileID <$> B.getFileStatus rawPath
                                FileSize path -> liftIO $ P.fileSize <$> getFileStatus path
                                GetDirectoryContents path -> liftIO $ D.getDirectoryContents path
                                IsDirectory path -> liftIO $ P.isDirectory <$> getFileStatus path
                                ListDirectory path -> liftIO $ D.listDirectory path
                                RemoveLink path -> liftIO $ P.removeLink path
