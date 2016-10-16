{-# OPTIONS_GHC -Wall #-}
module FDTools.Parse
  ( parseGraph
  ) where

import FDTools.Types
import Text.Parsec
import qualified Data.Set as S
import Data.Char (isSpace)

eol :: Parsec String st ()
eol = try (many1 endOfLine *> pure ()) <|> eof
comment :: Parsec String st ()
comment = spaces *> char '#' *> anyChar `manyTill` eol *> pure ()
graph :: Parsec String st Graph
graph = S.fromList <$> many1 (optional (try comment) *> edge <* (try comment <|> eol)) <* eof
edge :: Parsec String st Edge
edge = do
  p <- option "" $ try ((++".") <$> ident <* char ':' <* spaces)
  try $ do
    l <- vertexList p
    spaces >> (string "->" <|> string "→" <|> string "\\to") >> spaces
    r <- vertexList p
    return (l, r)
vertexList :: String -> Parsec String st VertexList
vertexList p = S.fromList <$>
   (
      optionMaybe (try (char '('))
   >> vertex p `sepBy` char ','
   <* optionMaybe (try (char ')'))
   )
vertex :: String -> Parsec String st Vertex
vertex p = Vertex . (p++) <$> ident
ident :: Parsec String st String
ident = trim <$> (many1 (letter <|> digit <|> oneOf " _.№") <|> string "∅")

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

parseGraph :: String -> Either ParseError Graph
parseGraph = parse graph "input"
