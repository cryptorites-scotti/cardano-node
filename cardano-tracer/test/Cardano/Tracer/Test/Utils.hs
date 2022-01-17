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
import           System.Info.Extra (isMac, isWindows)
import           Test.Tasty.QuickCheck

false :: String -> IO Property
false msg = return . counterexample msg $ property False

propRunInLogsStructure
  :: (FilePath -> FilePath -> IO Property)
  -> Property
propRunInLogsStructure testAction = ioProperty $ do
  (rootDir, deleteDir) <- newTempDir
  removeDirectoryContent rootDir
  (localSock, _) <- newTempFile
  let preparedLocalSock = prepareLocalSock localSock
  testAction rootDir preparedLocalSock
    `finally` (removeFile' preparedLocalSock >> deleteDir)

propRunInLogsStructure2
  :: (FilePath -> FilePath -> FilePath -> IO Property)
  -> Property
propRunInLogsStructure2 testAction = ioProperty $ do
  (rootDir, deleteDir) <- newTempDir
  (localSock1, _) <- newTempFile
  (localSock2, _) <- newTempFile
  let preparedLocalSock1 = prepareLocalSock localSock1
      preparedLocalSock2 = prepareLocalSock localSock2
  testAction rootDir preparedLocalSock1 preparedLocalSock2
    `finally` (   removeFile' preparedLocalSock1
               >> removeFile' preparedLocalSock2
               >> deleteDir)

prepareLocalSock :: FilePath -> FilePath
prepareLocalSock localSock =
  if isWindows
    then pipeForWindows
    else if isMac
           then sockForMac
           else localSock
 where
  pipeForWindows = "\\\\.\\pipe\\" <> dropDrive localSock
  sockForMac = "/tmp/cardano-tracer-test.pipe"

removeDirectoryContent :: FilePath -> IO ()
removeDirectoryContent dir = listDirectories dir >>= mapM_ removePathForcibly

removeFile' :: FilePath -> IO ()
removeFile' f = whenM (doesFileExist f) $ removeFile f

doesDirectoryEmpty :: FilePath -> IO Bool
doesDirectoryEmpty = fmap null . listDirectories
