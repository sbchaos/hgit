
module Data.Storage.ObjectTypes where

--import qualified Data.ByteString as B
--import qualified Data.ByteString.Char8 as C

data ObjectType = GCommit | GTree | GBlob | GTag
  deriving (Eq)

type ObjectId = String

type Size = Int

instance Show ObjectType where
  show GCommit = "commit"
  show GTree   = "tree"
  show GBlob   = "blob"
  show GTag    = "tag"

-- | The actual object type for git
type Blob = String

data TreeEntry = TreeEntry
  { mode :: String
  , gtype :: ObjectType
  , hash :: String
  , path :: String
  } deriving (Eq, Show)

data Tree = Tree
  { objectID :: ObjectId
  , tree     :: [TreeEntry]
  } deriving (Eq, Show)

data Commit = Commit
  { commitTree :: String
  , parents    :: [String]
--  , commitSha  :: String
  , author     :: String
  , committer  :: String
  , message    :: String
  } deriving (Eq, Show)

data Tag = Tag
  { tagRef  :: String
  , tagType :: String
  , tagName :: String
  , tagger  :: String
  , tagLog  :: String
} deriving (Eq,Show)

data Object = GoBlob   Size Blob
            | GoTree   Size [TreeEntry]
            | GoCommit Size Commit
            | GoTag    Size Tag
  deriving (Eq,Show)

