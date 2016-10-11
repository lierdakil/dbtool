{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module FDTools.Graphviz
  ( printGraph
  , showGraph
  ) where

import FDTools.Types
import FDTools.Util
import Data.Text.Lazy (unpack)
import Data.List
import Data.GraphViz
import Data.GraphViz.Types.Monadic
import qualified Data.Set as S
import Data.GraphViz.Attributes.Complete

printGraph :: Graph -> String
printGraph edges =
  let g = digraph (Str "G") $ mapM printEdge $ S.toList $ collect edges
  in unpack $ printDotGraph g

showGraph :: Graph -> IO String
showGraph edges = do
  let g = digraph (Str "G") $ mapM printEdge $ S.toList $ collect edges
  runGraphvizCanvas' g Xlib
  return $ unpack $ printDotGraph g

printEdge :: Edge -> DotM String ()
printEdge (l, r) | S.size l == 1
                 , ln <- S.elemAt 0 l = do
  mapM_ (\n -> vtxName ln --> vtxName n) r
  return ()
printEdge (l, r) = do
  nodeAttrs [rank SameRank]
  names <- mapM printVertex (S.toList l)
  let name = intercalate "," names
  node name [shape PointShape]
  mapM_ (\n -> edge n name [ArrowHead noArrow]) names
  mapM_ (\n -> name --> vtxName n) r
  return ()

printVertex :: Vertex -> DotM String String
printVertex (Vertex n) = node n [] >> return n
