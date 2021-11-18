{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric #-}
module Common (
   Template(..),
   Templates(..),
   newTemplate,
   addTemplate,
   setStatusToDone,
   mapTemplateWithKey,
   namedTemplates,
   Status(..) 
  ) where

import Data.Text (Text)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON, FromJSON)

data Status = Editable | Done deriving (Show, Eq, Generic)

data Template = Template{templateText :: Text, templateStatus :: Status} deriving (Show, Eq, Generic)

instance ToJSON Status
instance ToJSON Template
instance FromJSON Status
instance FromJSON Template

instance Ord Template where
    compare (Template t1 _ ) (Template t2 _) = compare t1 t2

type Templates = IM.IntMap Template 

newTemplate :: Text -> Template
newTemplate txt = Template txt Editable

addTemplate :: Template -> Templates -> Templates
addTemplate template templates = IM.insert (maybe 0 (succ . fst) (IM.lookupMax templates)) template templates

setStatusToDone :: Template -> Template
setStatusToDone template = template{templateStatus=Done}

mapTemplateWithKey :: Int -> (Template -> Template) -> Templates -> Templates
mapTemplateWithKey i f templates = IM.mapWithKey (\j t -> if j==i then f t else t) templates

namedTemplates :: M.Map Template Text
namedTemplates = M.fromAscList [(Template "t1 text here" Editable, "tname1"), (Template "t2 text" Done, "tname2")]
