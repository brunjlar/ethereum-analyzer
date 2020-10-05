{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

-- | Implementation of the ethereum-analyzer API.
module Ethereum.Analyzer.Web.Server.Handlers
  ( server
  ) where

import           Protolude                            hiding (Handler)

import           Control.Monad.Log                    (Severity, logInfo)
import           Data.Text.Prettyprint.Doc
import           Ethereum.Analyzer.EVM
import           Ethereum.Analyzer.Web.API            (API, DotCfgResp(..), RootPage(..), User(..), Users(..))
import qualified Ethereum.Analyzer.Web.Server.Logging as Log
import           Servant                              ((:<|>)(..), (:>), hoistServer, Raw, Server, ServerError)
import qualified Servant                              as S
import           Servant.Server.StaticFiles           (serveDirectoryFileServer)

-- | ethereum-analyzer API implementation.
server :: Severity -> Server (API :<|> "web" :> Raw)
server logLevel = hoistServer (Proxy :: Proxy API) (toHandler logLevel) handlers :<|> serveDirectoryFileServer "web"
  where
    handlers = pure RootPage :<|> users :<|> dotcfg :<|> dotcfg2

-- | Our custom handler type.
type Handler msg = ExceptT ServerError (Log.LogM msg IO)

-- | Translate our custom monad into a Servant handler.
--
-- See http://haskell-servant.readthedocs.io/en/stable/tutorial/Server.html#using-another-monad-for-your-handlers
-- for the details.
toHandler :: Pretty msg => Severity -> Handler msg a -> S.Handler a
toHandler logLevel = S.Handler . ExceptT . Log.withLogging logLevel . runExceptT

-- | Example endpoint.
users :: Handler Text Users
users = do
  logInfo ("Example of logging" :: Text)
  pure (Users [User 1 "Isaac" "Newton", User 2 "Albert" "Einstein"])

dotcfg :: Maybe Text -> Handler Text DotCfgResp
dotcfg (Just t) = pure (DotCfgResp (disasmToDotText $ EvmHexString t) "")
dotcfg _ = pure (DotCfgResp "" "")

dotcfg2 :: Maybe Text -> Handler Text DotCfgResp
dotcfg2 (Just t) = pure (uncurry DotCfgResp $ disasmToDotText2 $ EvmHexString t)
dotcfg2 _ = pure (DotCfgResp "" "")
