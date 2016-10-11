{-# OPTIONS_GHC -Wall #-}
module Types where

import Data.Set

type Vertex = String
type VertexList = Set Vertex
type Edge = (VertexList, VertexList)
type Graph = Set Edge
