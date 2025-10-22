{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Effectful.Unix
  (Unix
  , runUnix
  , createLink
  , fileID
  , fileSize
  , getDirectoryContents
  , isDirectory
  , listDirectory
  , removeLink
  , FileID
  , FileOffset)
where
import Effectful (Effect, Eff, (:>), IOE, liftIO)
import System.Posix.Types (FileID, FileOffset)
import Effectful.Dispatch.Dynamic (interpret)
import qualified System.Directory as D (getDirectoryContents, listDirectory)
import System.Posix.Files (getFileStatus)
import qualified System.Posix.Files as P (isDirectory, fileID, fileSize, createLink, removeLink)
import Effectful.TH (makeEffect)

data Unix :: Effect where
  CreateLink :: FilePath -> FilePath -> Unix m ()
  FileID :: FilePath -> Unix m FileID
  FileSize :: FilePath -> Unix m FileOffset
  GetDirectoryContents :: FilePath -> Unix m [FilePath]
  IsDirectory :: FilePath -> Unix m Bool
  ListDirectory :: FilePath -> Unix m [FilePath]
  RemoveLink :: FilePath -> Unix m ()

makeEffect ''Unix

runUnix :: IOE :> es => Eff (Unix : es) a -> Eff es a
runUnix = interpret $ \_ -> \case 
                                CreateLink path1 path2 -> liftIO $ P.createLink path1 path2
                                FileID path -> liftIO $ P.fileID <$> getFileStatus path
                                FileSize path -> liftIO $ P.fileSize <$> getFileStatus path
                                GetDirectoryContents path -> liftIO $ D.getDirectoryContents path
                                IsDirectory path -> liftIO $ P.isDirectory <$> getFileStatus path
                                ListDirectory path -> liftIO $ D.listDirectory path
                                RemoveLink path -> liftIO $ P.removeLink path
