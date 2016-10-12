{-# OPTIONS_GHC -Wall #-}
module ERTools.Types where

import Data.Map (Map)

data Attr = Attr
  { attrName :: String
  , attrIdent :: Bool
  } deriving (Show, Read)
data Entity = Entity
  { entName :: String
  , entParent :: Maybe String
  , entAttrs :: [Attr]
  } deriving (Show, Read)
data RelType = One | Many deriving (Show, Read)
data Rel = Rel
  { relName :: String
  , relConn :: [(RelType, String)]
  , relAttrs :: [Attr]
  } deriving (Show, Read)
data ER = ER
  { erEntities :: Map String Entity
  , erRels :: [Rel]
  } deriving (Show, Read)
