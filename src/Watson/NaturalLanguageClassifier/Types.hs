{-# LANGUAGE OverloadedStrings  #-}

module Watson.NaturalLanguageClassifier.Types
  where

import Data.Aeson
import Data.Text (Text)
import Network.API.Builder

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
