module Data.Storage.Parser where


import Text.ParserCombinators.Parsec

import Data.Storage.ObjectTypes
import Data.Parser.Number

nul :: Parser ()
nul = () <$ char '\0'

ws :: Parser String
ws = many $ oneOf " \n\t"

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


commit :: Parser Commit
commit = Commit <$> tre <*> parents <*> author <*> commiter <*> message
  where
    tre = string "tree " *> sha1 <* ws
    parents = many parent
    parent = string "parent " *> sha1 <* ws
    author = string "author " *> many (noneOf "\n") <* ws
    commiter = string "committer " *>  many (noneOf "\n") <* ws
    message = many anyChar


tag :: Parser Tag
tag = Tag <$> obj <*> typ <*> name <*> owner <*> tagmsg
  where
    obj    = string "object " *> sha1
    typ    = string "type "   *> many anyChar
    name   = string "tag "    *> many anyChar
    owner  = string "tagger " *> many anyChar
    tagmsg = many anyChar


gitObject :: Parser Object
gitObject = do
    (typ,len) <- header
    case typ of
        GBlob   -> GoBlob   len <$> blob len
        GTree   -> GoTree   len <$> treeP
        GCommit -> GoCommit len <$> commit
        GTag    -> GoTag len <$> tag


parseGitObject :: String -> Either ParseError Object
parseGitObject text = parse gitObject "test" text