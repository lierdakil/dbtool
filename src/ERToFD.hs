{-# LANGUAGE RecordWildCards #-}
module ERToFD where

import ERTools
import FDTools
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

erToFDs :: ER -> Graph
erToFDs ER{..} = S.fromList $
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
      = allKeys --> attrs2vtx' relName relAttrs
      : concatMap (uncurry pairFD) (pairs relConn)
      where
        allKeys = S.unions $ mapMaybe (\(_,en) -> entKeys <$> M.lookup en erEntities) relConn
        pairs (x:xs) = map ((,) x) xs ++ pairs xs
        pairs [] = []
        pairFD (One, enl) (One, enr) =
          [entNameKeys enl --> entNameKeys enr
          ,entNameKeys enr --> entNameKeys enl]
        pairFD (One, enl) (Many, enr) =
          [entNameKeys enr --> entNameKeys enl]
        pairFD (Many, enl) (One, enr) =
          [entNameKeys enl --> entNameKeys enr]
        pairFD (Many, enl) (Many, enr) =
          [(entNameKeys enl `S.union` entNameKeys enr) --> S.singleton (Vertex "∅")]
    keyAttrs = filter attrIdent
    attr2vtx' en attr = Vertex $ en ++ "." ++ attrName attr
    attrs2vtx' en = S.fromList . map (attr2vtx' en)
    attrs2vtx = attrs2vtx' . entName
    (-->) :: VertexList -> VertexList -> Edge
    (-->) a b = (a, b)
