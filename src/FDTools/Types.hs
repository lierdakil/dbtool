{-# OPTIONS_GHC -Wall #-}
module FDTools.Types where

import Data.Set

newtype Vertex = Vertex {vtxName :: String} deriving (Eq, Ord, Show)
type VertexList = Set Vertex
type Edge = (VertexList, VertexList)
type Graph = Set Edge
