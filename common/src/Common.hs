{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric #-}
module Common (
   NamedTemplate(..),
   Template(..),
   Templates(..),
   newTemplate,
   addTemplate,
   setStatusToDone,
   mapTemplateWithKey,
   Status(..),
   templateText,
   templateType,
   resetTemplate 
  ) where

import Data.Text (Text, pack, unpack, append)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON, FromJSON)

data Status = Editable | Done deriving (Show, Eq, Generic)
data Template = V String | L [Template] deriving (Show, Eq, Generic)
data NamedTemplate = NamedTemplate{templateName :: Text, templateStatus :: Status, template :: Template} deriving (Show, Eq, Generic)

instance ToJSON Template
instance FromJSON Template
instance ToJSON Status
instance ToJSON NamedTemplate
instance FromJSON Status
instance FromJSON NamedTemplate

instance Ord NamedTemplate where
    compare (NamedTemplate t1 _ _ ) (NamedTemplate t2 _ _) = compare t1 t2

type Templates = IM.IntMap Template 

newTemplate :: Text -> Template
newTemplate txt = V . unpack $ txt 

addTemplate :: Template -> Templates -> Templates
addTemplate template templates = IM.insert (maybe 0 (succ . fst) (IM.lookupMax templates)) template templates

setStatusToDone :: NamedTemplate -> NamedTemplate
setStatusToDone template = template{templateStatus=Done}

mapTemplateWithKey :: Int -> (Template -> Template) -> Templates -> Templates
mapTemplateWithKey i f templates = IM.mapWithKey (\j t -> if j==i then f t else t) templates

templateText :: Template -> Text
templateText (V s) = pack s
templateText (L ts) = foldr (append . templateType) "Types: " ts

templateType :: Template -> Text
templateType (V _) = "V"
templateType (L _) = "L"

resetTemplate :: Template -> Template
resetTemplate = const (V "")
