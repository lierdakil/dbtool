{-# OPTIONS_GHC -Wall #-}

module FDTools.Util
  ( minimize
  , collect
  , expand
  , fullext
  , superkeys
  , potkeys
  , nontrivial
  , project
  , nfbc
  , nf3
  , normalize
  ) where

import Types
import qualified Data.Set as S
import qualified Control.Arrow as A

minimize :: Graph -> Graph
minimize = collect . minimize' S.empty . nontrivial . expand
  where
    minimize' :: Graph -> Graph -> Graph
    minimize' acc es
      | es == S.empty = acc
      | otherwise =
        let
          -- find element with longest left side
          fdToTest = snd $ S.findMax $ S.map (\x -> (S.size $ fst x, x)) es
          es' = S.delete fdToTest es
          allFDs = es' `S.union` acc
        in
        if isDerivable fdToTest allFDs
        then minimize' acc es'
        else minimize' (S.insert fdToTest acc) es'
    isDerivable :: Edge -> Graph -> Bool
    isDerivable fd fds =
      let (l, r) = fd
          cls = closure l fds
      in r `S.isSubsetOf` cls

collect :: Graph -> Graph
collect g
  | S.null g = g
  | otherwise =
  let
    (l, _) = S.elemAt 0 g
    (c, gs) = S.partition ((==l) . fst) g
    collected = (l, unions $ S.map snd c)
  in
    S.insert collected $ collect gs

unions :: Ord a => S.Set (S.Set a) -> S.Set a
unions = S.foldl' S.union S.empty

expand :: Graph -> Graph
expand = S.foldl' (\acc (l,r) -> acc `S.union` S.map (\e -> (l, S.singleton e)) r) S.empty

closure :: VertexList -> Graph -> VertexList
closure x s =
  let
    c = x `S.union` c'
    c' = unions $ S.map snd $ S.filter (\z -> fst z `S.isSubsetOf` x) s
  in if c == x then c else closure c s

fullext :: Graph -> Graph
fullext g = S.map (closureToFDs g) allsubs
  where
    allvs = unions $ S.map (uncurry S.union) g
    allsubs = powerset allvs S.\\ S.empty

powerset :: Ord a => S.Set a -> S.Set (S.Set a)
powerset s
    | s == S.empty = S.singleton S.empty
    | otherwise = S.map (S.insert x) pxs `S.union` pxs
        where (x, xs) = S.deleteFindMin s
              pxs = powerset xs

closureToFDs :: Graph -> VertexList -> Edge
closureToFDs g x = (x, closure x g)

superkeys :: Graph -> Graph
superkeys g = S.filter (\x -> snd x == allvs) fe
  where
    allvs = unions $ S.map (uncurry S.union) g
    fe = fullext g

potkeys :: Graph -> Graph
potkeys g = S.filter (\x -> all (not . (`S.isProperSubsetOf` fst x) . fst) $ S.toList sk) sk
  where
    sk = superkeys g

nontrivial :: Graph -> Graph
nontrivial = S.map (\(f,s) -> (f, S.filter (not . (`S.member` f)) s)) . S.filter (not . uncurry (flip S.isSubsetOf))

project :: VertexList -> Graph -> Graph
project p = S.map (A.second (S.filter (`S.member` p))) . S.filter (\(f,_) -> f `S.isSubsetOf` p)

nfbc :: Graph -> Graph
nfbc g = S.filter (\(f,_) -> not $ f `S.member` sks) $ nontrivial $ fullext g
  where
    sks = S.map fst $ superkeys g

nf3 :: Graph -> Graph
nf3 g = S.filter (\(f,r) -> not $ f `S.member` sks || r `S.isSubsetOf` kas) $ nontrivial $ fullext g
  where
    sks = S.map fst $ superkeys g
    kas = unions $ S.map fst $ potkeys g

normalize :: Graph -> [[Vertex]]
normalize g = checkErr
  where
    invFD = minimize $ nfbc g
    restFD = S.difference (minimize $ fullext g) invFD
    invAttrR = unions $ S.map snd invFD
    restAttrL = unions $ S.map fst restFD
    invRels :: [[Vertex]]
    invRels = S.toList $ S.map (S.toList . uncurry S.union) invFD
    nf1 = unions $ S.map (uncurry S.union) g
    baseRel :: [Vertex]
    baseRel = S.toList $ S.filter (\x -> x `S.notMember` invAttrR || x `S.member` restAttrL) nf1
    basePrj = nontrivial $ project (S.fromList baseRel) $ fullext g
    result | S.null basePrj = invRels
           | otherwise =  baseRel : invRels
    prjFDs = S.unions $ map (\x -> project (S.fromList x) $ fullext g) result
    checkErr = if fullext prjFDs == fullext g
               then result
               else error "Failed"
