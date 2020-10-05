{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RecordWildCards
  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

-- | Serve the API as an HTTP server.
module Ethereum.Analyzer.Web.Server
  ( server
  , startApp
  ) where

import           Protolude                               hiding (option)

import           Control.Monad.Log                       (Severity(..))
import qualified Data.List                               as List
import           Data.Text                               (pack)
import           GHC.Stats                               (getRTSStatsEnabled)
import           Network.Wai.Handler.Warp                (Port, Settings, defaultSettings, runSettings, setBeforeMainLoop,
        setPort)
import qualified Network.Wai.Middleware.RequestLogger    as RL
import           Options.Applicative                     (ParserInfo, auto, eitherReader, execParser, fullDesc, header,
                                                          help, helper, info, long, metavar, option, progDesc, switch, value)
import qualified Prometheus                              as Prom
import qualified Prometheus.Metric.GHC                   as Prom
import           Servant                                 (serve)

import           Ethereum.Analyzer.Web.API               (apiraw)
import           Ethereum.Analyzer.Web.Server.Handlers   (server)
import           Ethereum.Analyzer.Web.Server.Instrument (defaultPrometheusSettings, prometheus, requestDuration)
import qualified Ethereum.Analyzer.Web.Server.Logging    as Log

-- | Configuration for the application.
data Config = Config
  { port :: Port
  , accessLogs :: AccessLogs
  , logLevel :: Severity
  , enableGhcMetrics :: Bool
  } deriving (Show)

-- | What level of access logs to show.
data AccessLogs
  = Disabled -- ^ Don't show access logs.
  | Enabled -- ^ Show Apache-style access logs.
  | DevMode -- ^ Show detailed, colorful access logs. Not suitable in production.
  deriving (Eq, Show)

-- | Run the service.
startApp :: IO ()
startApp = runApp =<< execParser options

options :: ParserInfo Config
options = info (helper <*> parser) description
  where
    parser =
      Config <$>
      option auto (fold [long "port", metavar "PORT", help "Port to listen on"]) <*>
      option
        (eitherReader parseAccessLogs)
        (fold
           [long "access-logs", help "How to log HTTP access", value Disabled]) <*>
      option
        (eitherReader
           (maybe (throwError (toS invalidLogLevel)) pure .
            Log.fromKeyword . toS))
        (fold
           [ long "log-level"
           , help "Minimum severity for log messages"
           , value Informational
           ]) <*>
      switch
        (fold
           [ long "ghc-metrics"
           , help "Export GHC metrics. Requires running with +RTS."
           ])
    invalidLogLevel = "Log level must be one of: " <> allLogLevels
    allLogLevels =
      fold . List.intersperse "," . List.map Log.toKeyword $ enumFrom minBound
    parseAccessLogs "none" = pure Disabled
    parseAccessLogs "basic" = pure Enabled
    parseAccessLogs "dev" = pure DevMode
    parseAccessLogs _ = throwError "One of 'none', 'basic', or 'dev'"
    description =
      fold [fullDesc, progDesc "Ethereum Analyzer", header "ethereum-analyzer"]

runApp :: Config -> IO ()
runApp config@Config {..} = do
  requests <- Prom.registerIO requestDuration
  when enableGhcMetrics $ do
    statsEnabled <- getRTSStatsEnabled
    unless statsEnabled $
      Log.withLogging logLevel $
      Log.log
        Warning
        ("Exporting GHC metrics but GC stats not enabled. Re-run with +RTS -T." :: Text)
    void $ Prom.register Prom.ghcMetrics
  runSettings settings (middleware requests)
  where
    settings = warpSettings config
    middleware r =
      logging . prometheus defaultPrometheusSettings r "ethereum_analyzer" $ app
    logging =
      case accessLogs of
        Disabled -> identity
        Enabled -> RL.logStdout
        DevMode -> RL.logStdoutDev
    app = serve apiraw (server logLevel)

-- | Generate warp settings from config
--
-- Serve from a port and print out where we're serving from.
warpSettings :: Config -> Settings
warpSettings Config {..} =
  setBeforeMainLoop
    (Log.withLogging logLevel printPort)
    (setPort port defaultSettings)
  where
    printPort = Log.log Informational (pack $ "Listening on :" <> show port)
