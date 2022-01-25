{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Chairman.Commands.Run
  ( cmdRun
  ) where

import           Cardano.Prelude hiding (option)

import           Control.Monad.Class.MonadTime (DiffTime)
import           Control.Tracer (Tracer (..), stdoutTracer)
import qualified Data.Text as Text
import qualified Data.Time.Clock as DTC
import           Options.Applicative
import qualified Options.Applicative as Opt
import qualified System.IO as IO

import           Cardano.Node.Configuration.NodeAddress
import           Cardano.Node.Configuration.POM (makeNodeConfiguration, parseNodeConfigurationFP,
                   pncProtocol)
import           Cardano.Node.Protocol.Types (Protocol (..))
import           Cardano.Node.Types
import           Ouroboros.Consensus.Config.SecurityParam (SecurityParam (..))

import           Cardano.Api
import           Cardano.Api.Protocol.Cardano
import           Cardano.Api.Protocol.Shelley
import           Cardano.Chairman (chairmanTest)

data RunOpts = RunOpts
  { -- | Stop the test after given number of seconds. The chairman will
    -- observe only for the given period of time, and check the consensus
    -- and progress conditions at the end.
    --
    caRunningTime :: !DiffTime
    -- | Expect this amount of progress (chain growth) by the end of the test.
  , caMinProgress :: !BlockNo
  , caSocketPaths :: ![SocketPath]
  , caConfigYaml :: !ConfigYamlFilePath
  , caConsensusMode :: !AnyConsensusModeParams
  , caNetworkMagic :: !NetworkMagic
  }

parseConfigFile :: Parser FilePath
parseConfigFile =
  strOption
    ( long "config"
    <> metavar "NODE-CONFIGURATION"
    <> help "Configuration file for the cardano-node"
    <> completer (bashCompleter "file")
    )

parseSocketPath :: Text -> Parser SocketPath
parseSocketPath helpMessage =
  SocketPath <$> strOption
    ( long "socket-path"
    <> help (toS helpMessage)
    <> completer (bashCompleter "file")
    <> metavar "FILEPATH"
    )

parseRunningTime :: Parser DiffTime
parseRunningTime =
  option ((fromIntegral :: Int -> DiffTime) <$> auto)
    (  long "timeout"
    <> short 't'
    <> metavar "SECONDS"
    <> help "Run the chairman for this length of time in seconds."
    )


parseTestnetMagic :: Parser NetworkMagic
parseTestnetMagic =
  NetworkMagic <$>
    Opt.option Opt.auto
      (  Opt.long "testnet-magic"
      <> Opt.metavar "INT"
      <> Opt.help "The testnet network magic number"
      )

pConsensusModeParams :: Parser AnyConsensusModeParams
pConsensusModeParams = asum
  [ Opt.flag' (AnyConsensusModeParams ShelleyModeParams)
      (  Opt.long "shelley-mode"
      <> Opt.help "For talking to a node running in Shelley-only mode."
      )
  , Opt.flag' ()
      (  Opt.long "byron-mode"
      <> Opt.help "For talking to a node running in Byron-only mode."
      )
       *> pByronConsensusMode
  , Opt.flag' ()
      (  Opt.long "cardano-mode"
      <> Opt.help "For talking to a node running in full Cardano mode (default)."
      )
       *> pCardanoConsensusMode
  , -- Default to the Cardano consensus mode.
    pure . AnyConsensusModeParams . CardanoModeParams $ EpochSlots defaultByronEpochSlots
  ]
 where
   pCardanoConsensusMode :: Parser AnyConsensusModeParams
   pCardanoConsensusMode = AnyConsensusModeParams . CardanoModeParams <$> pEpochSlots

   pByronConsensusMode :: Parser AnyConsensusModeParams
   pByronConsensusMode = AnyConsensusModeParams . ByronModeParams <$> pEpochSlots

   defaultByronEpochSlots :: Word64
   defaultByronEpochSlots = 21600

parseProgress :: Parser BlockNo
parseProgress =
  option ((fromIntegral :: Int -> BlockNo) <$> auto)
    (  long "require-progress"
    <> short 'p'
    <> metavar "INT"
    <> help "Require this much chain-growth progress, in blocks."
  )



parseRunOpts :: Parser RunOpts
parseRunOpts =
  RunOpts
  <$> parseRunningTime
  <*> parseProgress
  <*> some (parseSocketPath "Path to a cardano-node socket")
  <*> fmap ConfigYamlFilePath parseConfigFile
  <*> pConsensusModeParams
  <*> parseTestnetMagic

run :: RunOpts -> IO ()
run RunOpts
    { caRunningTime
    , caMinProgress
    , caSocketPaths
    , caConfigYaml
    , caConsensusMode
    , caNetworkMagic
    } = do

  configYamlPc <- liftIO . parseNodeConfigurationFP $ Just caConfigYaml

  nc <- case makeNodeConfiguration configYamlPc of
            Left err -> panic $ "Error in creating the NodeConfiguration: " <> Text.pack err
            Right nc' -> return nc'

  let localNodeCliParams = mkLocalNodeClientParams caConsensusMode const

  chairmanTest
    (timed stdoutTracer)
    localNodeCliParams
    caNetworkMagic
    caConsensusMode
    caRunningTime
    caMinProgress
    caSocketPaths

  return ()

timed :: Tracer IO a -> Tracer IO a
timed (Tracer runTracer) = Tracer $ \a -> do
  ts <- DTC.getCurrentTime
  IO.putStr ("[" <> show ts <> "] ")
  runTracer a

cmdRun :: Mod CommandFields (IO ())
cmdRun = command "run"  $ flip info idm $ run <$> parseRunOpts
