{-# LANGUAGE OverloadedStrings #-}

module Watson.NaturalLanguageClassifier
  ( module Watson.NaturalLanguageClassifier
  , module Watson.NaturalLanguageClassifier.Types)
  where

import Watson.NaturalLanguageClassifier.Types
import Watson.Util.Multipart

import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Network.API.Builder
import Network.API.Builder.Send.Multipart (Multipart(..))
import Network.HTTP.Client (Response(..), applyBasicAuth)
import Network.HTTP.Client.MultipartFormData

apiEndpoint =
  "https://gateway.watsonplatform.net/natural-language-classifier/api/v1"

naturalLanguageClassifier :: Text -- ^ Service username
                          -> Text -- ^ Service password
                          -> NLC
naturalLanguageClassifier username password =
  let builder = Builder { _name = "NaturalLanguageClassifier API"
                        , _baseURL = apiEndpoint
                        , _customizeRoute = id
                        , _customizeRequest =
                            applyBasicAuth (TE.encodeUtf8 username)
                                           (TE.encodeUtf8 password)
                        }
  in NLC builder

clfList :: NLC -> IO (Either (APIError ()) Classifiers)
clfList (NLC builder) =
  let route = Route [ "classifiers" ] [] "GET"
      api = runRoute route
  in execAPI builder () api

clfCreate :: NLC
          -> FilePath -- ^ CSV with training data
          -> TrainingMetadata
          -> IO (Either (APIError ()) Classifiers)
clfCreate (NLC builder) trainData trainMetadata =
  let route = Route [ "classifiers" ] [] "POST"
      parts = [ partFile "training_data" trainData
              , partLBS "training_metadata" (encode trainMetadata)
              ]
      api = runRouteMP (Multipart parts) route
  in execAPI builder () api

clfClassify :: NLC
            -> Classifier
            -> Text
            -> IO (Either (APIError ()) ClassifierOutput)
clfClassify (NLC builder) clf text =
  let route = Route [ "classifiers", classifierId clf, "classify" ]
                    [ "text" =. text]
                    "GET"
      api = runRoute route
  in execAPI builder () api

clfStatus :: NLC -> Classifier -> IO (Either (APIError ()) ClassifierStatus)
clfStatus (NLC builder) clf =
  let route = Route [ "classifiers", classifierId clf ] [] "GET"
      api = runRoute route
  in execAPI builder () api

clfRemove :: NLC
          -> Classifier
          -> IO (Either (APIError ()) (Response BL.ByteString))
clfRemove (NLC builder) clf =
  let route = Route [ "classifiers", classifierId clf ] [] "DELETE"
      api = routeResponse route
  in execAPI builder () api
