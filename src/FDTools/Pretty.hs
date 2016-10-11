{-# OPTIONS_GHC -Wall #-}
module FDTools.Pretty where

import FDTools.Types
import Data.List
import qualified Data.Set as S

graphToString :: Graph -> String
graphToString = intercalate "\n" . map edgeToString . S.toList

edgeToString :: Edge -> String
edgeToString (l, r) = vertexListToString l ++ " -> " ++ vertexListToString r

vertexListToString :: VertexList -> String
vertexListToString vl
  | S.size vl == 0 = "()"
  | S.size vl == 1 = vtxName $ S.elemAt 0 vl
  | otherwise = "(" ++ intercalate ", " (map vtxName $ S.toList vl) ++ ")"
