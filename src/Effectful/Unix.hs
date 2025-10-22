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
  , fileID
  , fileSize
  , getDirectoryContents
  , isDirectory
  , listDirectory
  , FileID
  , FileOffset)
where
import Effectful (Effect, Eff, (:>), IOE, liftIO)
import System.Posix.Types (FileID, FileOffset)
import Effectful.Dispatch.Dynamic (interpret)
import qualified System.Directory as D (getDirectoryContents, listDirectory)
import System.Posix.Files (getFileStatus)
import qualified System.Posix.Files as P (isDirectory, fileID, fileSize)
import Effectful.TH (makeEffect)

data Unix :: Effect where
  FileID :: FilePath -> Unix m FileID
  FileSize :: FilePath -> Unix m FileOffset
  GetDirectoryContents :: FilePath -> Unix m [FilePath]
  IsDirectory :: FilePath -> Unix m Bool
  ListDirectory :: FilePath -> Unix m [FilePath]

makeEffect ''Unix

runUnix :: IOE :> es => Eff (Unix : es) a -> Eff es a
runUnix = interpret $ \_ -> \case 
                                FileID path -> liftIO $ P.fileID <$> getFileStatus path
                                FileSize path -> liftIO $ P.fileSize <$> getFileStatus path
                                GetDirectoryContents path -> liftIO $ D.getDirectoryContents path
                                IsDirectory path -> liftIO $ P.isDirectory <$> getFileStatus path
                                ListDirectory path -> liftIO $ D.listDirectory path
