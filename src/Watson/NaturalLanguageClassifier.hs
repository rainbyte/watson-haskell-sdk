{-# LANGUAGE OverloadedStrings #-}

module Watson.NaturalLanguageClassifier
  where

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

type ClassifierId = Text

data Classifier = Classifier
  { name :: Maybe Text
  , language :: Maybe Text
  , url :: Text
  , classifierId :: Text
  , created :: Maybe Text
  }
  deriving (Eq, Show)

instance FromJSON Classifier where
  parseJSON (Object o) =
    Classifier <$> o .: "name"
               <*> o .: "language"
               <*> o .: "url"
               <*> o .: "classifier_id"
               <*> o .: "created"
  parseJSON _ = mempty

newtype Classifiers = Classifiers [Classifier]
  deriving (Eq, Show)

instance FromJSON Classifiers where
  parseJSON (Object o) = Classifiers <$> o .: "classifiers"
  parseJSON _ = mempty

instance Receivable Classifiers where
  receive = useFromJSON

data Status = NonExistent
            | Training
            | Failed
            | Available
            | Unavailable
            | Unknown
  deriving (Eq, Show)

instance FromJSON Status where
  parseJSON (String s) =
    pure (createStatus s)
    where
      createStatus :: Text -> Status
      createStatus str =
        case str of
          "Non Existent" -> NonExistent
          "Training" -> Training
          "Failed" -> Failed
          "Available" -> Available
          "Unavailable" -> Unavailable
          _ -> Unknown
  parseJSON _ = mempty

data ClassifierStatus = ClassifierStatus
  { classifier :: Classifier
  , status :: Maybe Status
  , statusDescription :: Maybe Text
  }
  deriving (Eq, Show)

instance FromJSON ClassifierStatus where
  parseJSON (Object o) =
    let clf = Classifier <$> o .: "name"
                         <*> o .: "language"
                         <*> o .: "url"
                         <*> o .: "classifier_id"
                         <*> o .: "created"
    in ClassifierStatus <$> clf
                        <*> o .: "status"
                        <*> o .: "status_description"
  parseJSON _ = mempty

instance Receivable ClassifierStatus where
  receive = useFromJSON

data ClassifierClass = ClassifierClass
  { confidence :: Maybe Double
  , className :: Maybe Text
  }
  deriving (Eq, Show)

instance FromJSON ClassifierClass where
  parseJSON (Object o) =
    ClassifierClass <$> o .: "confidence"
                    <*> o .: "class_name"
  parseJSON _ = mempty

data ClassifierOutput = ClassifierOutput
  { coClassifierId :: Maybe Text
  , coUrl :: Maybe Text
  , coText :: Maybe Text
  , coTopClass :: Maybe Text
  , coClasses :: Maybe [ClassifierClass]
  }
  deriving (Eq, Show)

instance FromJSON ClassifierOutput where
  parseJSON (Object o) =
    ClassifierOutput <$> o .: "classifier_id"
                     <*> o .: "url"
                     <*> o .: "text"
                     <*> o .: "top_class"
                     <*> o .: "classes"
  parseJSON _ = mempty

instance Receivable ClassifierOutput where
  receive = useFromJSON

data TrainingMetadata = TrainingMetadata
  { tmName :: Maybe Text
  , tmLanguage :: Text
  }
  deriving (Eq, Show)

instance ToJSON TrainingMetadata where
  toJSON (TrainingMetadata name language) =
    object ["name" .= name, "language" .= language]

newtype NLC = NLC Builder

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
  let route = Route [ "classifiers", classifierId clf, "classify" ] [] "POST"
      parts = [ partBS "text" (TE.encodeUtf8 text) ]
      api = runRouteMP (Multipart parts) route
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
