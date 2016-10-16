{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module ERTools.Graphviz
  ( showER
  ) where

import ERTools.Types
import Data.Text.Lazy (pack, unpack)
import Data.GraphViz
import Data.GraphViz.Types.Monadic
import qualified Data.GraphViz.Attributes.HTML as H
import Data.GraphViz.Attributes.Complete

whenJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
whenJust = flip $ maybe (return ())

showER :: ER -> String
showER ER{..} =
  let
    g = graph (Str "G") $ do
      graphAttrs [Layout Neato, Overlap VoronoiOverlap, Splines SplineEdges]
      mapM_ entNode erEntities
      mapM_ relNode erRels
  in unpack $ printDotGraph g

entNode :: Entity -> DotM String ()
entNode Entity{..} = do
  node entName [shape BoxShape]
  mapM_ (attrNode entName) entAttrs
  whenJust entParent $ \p -> do
    let pn = "parent::" ++ p
    node pn [shape Triangle, textLabel ""]
    entName --> pn
    p --> pn

attrNode :: String -> Attr -> DotM String ()
attrNode p Attr{..} = do
  let nid = p ++ ":" ++ attrName
      lab | attrIdent = toLabel [H.Format H.Underline [H.Str $ pack attrName]]
          | otherwise = toLabel attrName
  node nid [lab]
  p --> nid

relNode :: Rel -> DotM String ()
relNode Rel{..} = do
  node relName [shape DiamondShape]
  mapM_ (\(typ, ent) -> edge relName ent [HeadLabel $ toLabelValue $ ct2s typ]) relConn
  mapM_ (attrNode relName) relAttrs
  where
    ct2s :: RelType -> String
    ct2s One = "1"
    ct2s Many = "M"
