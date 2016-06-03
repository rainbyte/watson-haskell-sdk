{-# LANGUAGE OverloadedStrings #-}

module Watson.RelationshipExtraction
  where

import           Control.Lens ((&), (.~), (?~), (^.))
import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import           Data.Text (Text)
import qualified Data.Text.Encoding as TE
import           Network.Wreq

apiEndpoint =
  "https://gateway.watsonplatform.net/relationship-extraction-beta/api"


data OutputFormat = OutputJSON | OutputXML
  deriving Show

outputFormatRepr :: OutputFormat -> Text
outputFormatRepr OutputJSON = "json"
outputFormatRepr OutputXML = "xml"


data DatasetLang = DatasetEnglish | DatasetSpanish
  deriving Show

datasetRepr :: DatasetLang -> Text
datasetRepr DatasetEnglish = "ie-en-news"
datasetRepr DatasetSpanish = "ie-es-news"


data Config = Config
  { username :: Text
  , password :: Text
  , dataset :: DatasetLang
  , outputFormat :: OutputFormat
  }
  deriving Show

mkConfig :: Text -> Text -> Config
mkConfig username' password' = Config
  { username = username'
  , password = password'
  , dataset = DatasetEnglish
  , outputFormat = OutputXML
  }


extract :: Config -> Text -> IO ByteString
extract config txt = do
  let authData = auth ?~ basicAuth (TE.encodeUtf8 $ username config)
                                   (TE.encodeUtf8 $ password config)
      params' = [ "sid" := datasetRepr (dataset config)
                , "txt" := txt
                , "rt" := outputFormatRepr (outputFormat config)
                ]
      opts = defaults & authData
  r <- postWith opts (apiEndpoint ++ "/v1/sire/0") params'
  pure $ r ^. responseBody
