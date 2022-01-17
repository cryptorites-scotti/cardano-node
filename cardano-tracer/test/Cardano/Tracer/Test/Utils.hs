module Cardano.Tracer.Test.Utils
  ( doesDirectoryEmpty
  , false
  , propRunInLogsStructure
  , propRunInLogsStructure2
  , removeDirectoryContent
  ) where

import           Control.Exception (finally)
import           Control.Monad.Extra (whenM)
import           System.Directory.Extra (doesFileExist, listDirectories,
                   removeFile, removePathForcibly)
import           System.FilePath (dropDrive)
import           System.IO.Extra (newTempDir, newTempFile)
import           System.Info.Extra (isWindows)
import           Test.Tasty.QuickCheck

false :: String -> IO Property
false msg = return . counterexample msg $ property False

propRunInLogsStructure
  :: (FilePath -> FilePath -> IO Property)
  -> Property
propRunInLogsStructure testAction = ioProperty $ do
  (rootDir, deleteDir) <- newTempDir
  (localSock, _) <- newTempFile
  let preparedLocalSock = if isWindows then forWindows localSock else "/tmp/cardano-tracer-test.pipe" -- localSock
  testAction rootDir preparedLocalSock
    `finally` (removeFile' preparedLocalSock >> deleteDir)

propRunInLogsStructure2
  :: (FilePath -> FilePath -> FilePath -> IO Property)
  -> Property
propRunInLogsStructure2 testAction = ioProperty $ do
  (rootDir, deleteDir) <- newTempDir
  (localSock1, _) <- newTempFile
  (localSock2, _) <- newTempFile
  let preparedLocalSock1 = if isWindows then forWindows localSock1 else localSock1
      preparedLocalSock2 = if isWindows then forWindows localSock2 else localSock2
  testAction rootDir preparedLocalSock1 preparedLocalSock2
    `finally` (   removeFile' preparedLocalSock1
               >> removeFile' preparedLocalSock2
               >> deleteDir)

forWindows :: FilePath -> FilePath
forWindows localSock = "\\\\.\\pipe\\" <> dropDrive localSock

removeDirectoryContent :: FilePath -> IO ()
removeDirectoryContent dir = listDirectories dir >>= mapM_ removePathForcibly

removeFile' :: FilePath -> IO ()
removeFile' f = whenM (doesFileExist f) $ removeFile f

doesDirectoryEmpty :: FilePath -> IO Bool
doesDirectoryEmpty = fmap null . listDirectories
