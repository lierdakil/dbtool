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
import Data.Maybe
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
  names <- catMaybes <$> mapM printVertex (S.toList r)
  mapM_ (\n -> vtxName ln --> n) names
  return ()
printEdge (l, r) = do
  namesl <- catMaybes <$> mapM printVertex (S.toList l)
  namesr <- catMaybes <$> mapM printVertex (S.toList r)
  let name = intercalate "," namesl
  node name [shape PointShape]
  mapM_ (\n -> edge n name [ArrowHead noArrow]) namesl
  mapM_ (\n -> name --> n) namesr
  return ()

printVertex :: Vertex -> DotM String (Maybe String)
printVertex (Vertex n)
  | n == "∅" = return Nothing
  | otherwise = node n [] >> return (Just n)
