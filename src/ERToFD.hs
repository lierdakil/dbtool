{-# LANGUAGE RecordWildCards #-}
module ERToFD where

import ERTools
import FDTools
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

erToFDs :: ER -> Graph
erToFDs ER{..} = collect . nontrivial . S.fromList $
  entityFDs ++ relFDs
  where
    entityFDs = map (entityFD . snd) $ M.toList erEntities
    entityFD x
      = entKeys x --> attrs2vtx x (entAttrs x)
    entNameKeys = maybe S.empty entKeys . flip M.lookup erEntities
    entKeys x
      | null kas = attrs2vtx p aas
      | otherwise = attrs2vtx p kas
      where kas = keyAttrs aas
            aas = entAttrs p
            p = topParent x
    topParent x@Entity{..}
      | Just p <- entParent >>= \pn -> M.lookup pn erEntities
      = topParent p
      | otherwise = x
    relFDs = concatMap relFD erRels
    relFD Rel{..}
      | null $ ctKeys Many
      = oneToOne
      | otherwise
      = ctKeys Many --> rsOrNull : oneToOne
      where
        oneToOne = map (\x -> entKeys x --> rs) (ctf One)
        rs = S.unions [ctKeys One, attrs2vtx' relName relAttrs]
        rsOrNull | S.null rs = S.singleton (Vertex "∅")
                 | otherwise = rs
        allKeys = S.unions $ map (entNameKeys . snd) relConn
        ctf t = mapMaybe (flip M.lookup erEntities . snd) $ filter ((t ==) . fst) relConn
        ctKeys t = getKeys $ ctf t
        getKeys = S.unions . map entKeys
    keyAttrs = filter attrIdent
    attr2vtx' en attr = Vertex $ en ++ "." ++ attrName attr
    attrs2vtx' en = S.fromList . map (attr2vtx' en)
    attrs2vtx = attrs2vtx' . entName
    (-->) :: VertexList -> VertexList -> Edge
    (-->) a b = (a, b)
