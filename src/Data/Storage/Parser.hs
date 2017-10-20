module Data.Storage.Parser where


import Text.ParserCombinators.Parsec

import Data.Storage.ObjectTypes
import Data.Parser.Number

nul :: Parser ()
nul = () <$ char '\0'

ws :: Parser String
ws = many $ char ' '

gitType :: Parser ObjectType
gitType =  GBlob   <$ string "blob"
       <|> GTree   <$ string "tree"
       <|> GCommit <$ string "commit"
       <|> GTag    <$ string "tag"

header :: Parser (ObjectType, Int)
header = (,) <$> (gitType <* ws) <*> (int <* nul)

blob :: Int -> Parser Blob
blob n = count n anyChar

sha1 :: Parser String
sha1 = count 40 anyChar

entry :: Parser TreeEntry
entry = TreeEntry <$> (filetyp <* ws) <*> (gitType <* ws) <*> (sha1 <* ws) <*> filepath
  where
    filetyp = count 6 anyChar
    filepath = many1 $ noneOf "\0"
    gtype = gitType

treeP :: Parser [TreeEntry]
treeP = many1 entry
