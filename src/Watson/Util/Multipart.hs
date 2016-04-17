module Watson.Util.Multipart
  where

import Control.Exception
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Bifunctor
import Network.API.Builder
import Network.API.Builder.Send.Multipart
import Network.HTTP.Client

eitherOr :: Maybe a -> b -> Either b a
a `eitherOr` b =
  case a of
    Just x -> Right x
    Nothing -> Left b

runRouteMP :: (MonadIO m, ErrorReceivable e, Receivable r)
           => Multipart -> Route -> APIT s e m r
runRouteMP mp r = do
  builder <- liftBuilder get
  manager <- liftManager ask
  req <- ExceptT $ do
    maybeReq <- liftIO $ sendMultipart builder r mp
    pure $ maybeReq `eitherOr` InvalidURLError
  response <- liftIO $ try $ httpLbs req manager
  res <- ExceptT $ pure $ first HTTPError response
  ExceptT $ pure $ receive res
