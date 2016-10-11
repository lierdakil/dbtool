{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Main where

import FDTools
import Options.Applicative
import Control.Monad
import Data.List.Split
import qualified Data.Set as S
import Data.List

commands :: Parser (Graph -> Graph)
commands =
  subparser $
       command "collect" (info (pure collect) idm)
    <> command "minimize" (info (pure minimize) idm)
    <> command "expand" (info (pure expand) idm)
    <> command "fullext" (info (pure fullext) idm)
    <> command "superkeys" (info (pure superkeys) idm)
    <> command "potkeys" (info (pure potkeys) idm)
    <> command "potkeys" (info (pure potkeys) idm)
    <> command "nontrivial" (info (pure nontrivial) idm)
    <> command "project" (info (project <$> attr) idm)
    <> command "nfbc" (info (pure nfbc) idm)
    <> command "nf3" (info (pure nf3) idm)
    -- <> command "normalize" (info (pure normalize) idm)
    where
      attr = S.fromList . map Vertex . splitOn "," <$> argument str (metavar "ATTR,ATTR...")

normcmd :: Parser (Graph -> [[Vertex]])
normcmd =
    flag' () (long "normalize") *> pure normalize

output :: Parser (Graph -> IO String)
output = if' showGraph (return . graphToString) <$> switch (long "dot")
  where if' a _ True  = a
        if' _ b False = b

input :: Parser (IO Graph)
input = fmap (either (error . show) id . parseGraph) . readFile <$> argument str (metavar "FILE")

showRels :: [[Vertex]] -> String
showRels = intercalate "\n" . map (intercalate ", " . map vtxName)

main :: IO ()
main = join (execParser opts) >>= putStrLn
  where
    opts = info (
       (((fmap showRels .) <$> (fmap <$> normcmd))
      <|> (((.) . (=<<) <$> output) <*> (fmap . foldl (flip (.)) id <$> many commands)))
      <*> input
      )
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )
