{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
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
   resetTemplate,
   MTemplate(..),    
   Template'(..),
   toITemplate,
   pruneToTemplate,
   toMTemplate
  ) where

import Data.Text (Text, pack, unpack, append)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON, FromJSON)
import Data.Functor.Identity (Identity (Identity))

data Status = Editable | Done deriving (Show, Read,  Eq, Generic)
--data Template = V String | L [Template] deriving (Show, Read, Eq, Generic)
data NamedTemplate = NamedTemplate{templateName :: Text, templateStatus :: Status, template :: Template} deriving (Show, Read, Eq, Generic)

data Template' m = V (ReduceIdentity m String) | L (ReduceIdentity m [Template' m]) 
deriving instance (Eq (ReduceIdentity m String), Eq (ReduceIdentity m [Template' m])) => Eq (Template' m)
deriving instance (Show (ReduceIdentity m String), Show (ReduceIdentity m [Template' m])) => Show (Template' m)
deriving instance (Generic (ReduceIdentity m String), Generic (ReduceIdentity m [Template' m])) => Generic (Template' m)
deriving instance (Read (ReduceIdentity m String), Read (ReduceIdentity m [Template' m])) => Read (Template' m)

type MTemplate = Template' Maybe
type Template = Template' Identity

type family ReduceIdentity m a where
    ReduceIdentity Identity a = a
    ReduceIdentity m a = m a


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

resetTemplate :: MTemplate -> MTemplate
resetTemplate = const (V $ Just "")

toITemplate :: MTemplate -> Maybe Template
toITemplate (V Nothing) = Nothing
toITemplate (V (Just s)) = Just $ V s
toITemplate (L Nothing) = Nothing
toITemplate (L (Just ts)) = case traverse toITemplate ts of
    Nothing -> Nothing
    Just xs -> Just (L xs)

pruneToTemplate :: MTemplate -> Maybe Template
pruneToTemplate (V Nothing) = Nothing
pruneToTemplate (V (Just s)) = Just $ V s
pruneToTemplate (L Nothing) = Nothing
pruneToTemplate (L (Just ts)) = let
    pruneL [] = []
    pruneL (y:ys) = case toITemplate y of
        Nothing -> pruneL ys 
        Just z -> z : pruneL ys
    in
    case pruneL ts of
        [] -> Nothing
        xs -> Just $ L xs

toMTemplate :: Template -> MTemplate
toMTemplate (V s) = V $ Just s
toMTemplate (L ts) = L $ Just $ map toMTemplate ts
