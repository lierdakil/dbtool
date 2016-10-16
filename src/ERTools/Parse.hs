{-# OPTIONS_GHC -Wall #-}
module ERTools.Parse
  ( parseER
  ) where

import ERTools.Types
import Text.Parsec
import Data.Either
import qualified Data.Map as M
import Data.Char (isSpace)

eol :: Parsec String st ()
eol = try (many1 endOfLine *> pure ()) <|> eof

spaces' :: Parsec String st ()
spaces' = do
  _ <- many $ char ' '
  return ()

ident :: Parsec String st String
ident = trim <$> many1 (letter <|> digit <|> oneOf " _.№")

er :: Parsec String st ER
er = do
  spaces'
  (rels, ents) <- partitionEithers <$> many ((Right <$> entity) <|> (Left <$> rel))
  spaces'
  eof
  let entMap = M.fromList $ map (\x -> (entName x, x)) ents
  return $ ER entMap rels

entity :: Parsec String st Entity
entity = do
  _ <- char '*'
  name <- ident
  parent <- optionMaybe $ try $ do
    spaces
    _ <- char ':'
    spaces
    ident
  eol
  attrs <- many attr
  return $ Entity name parent attrs

rel :: Parsec String st Rel
rel = do
  _ <- char '='
  name <- ident
  conns <- many $ try $ do
    spaces
    _ <- char ':'
    spaces
    ct <- (char '1' *> pure One) <|> (oneOf "M*" *> pure Many)
    _ <- many1 space
    ent <- ident
    return (ct, ent)
  eol
  attrs <- many attr
  return $ Rel name conns attrs

attr :: Parsec String st Attr
attr = Attr <$> ident <*> option False (char '*' *> pure True) <* eol

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

parseER :: String -> Either ParseError ER
parseER = parse er "input"
