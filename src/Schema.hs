{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ImpredicativeTypes    #-}

module Schema where

import           Database.Beam
import           Database.Beam.Backend
import           Database.Beam.Migrate
-- import           Database.Beam.Postgres
import           Database.Beam.Sqlite
import           Database.SQLite.Simple (open, Connection)

import           Data.Int               (Int64)
import           Data.Text              (Text)
import           Data.Time              (UTCTime)
import           Data.UUID              (UUID)

data UserT f = User
  { _userId         :: Columnar f Int64
  , _userName       :: Columnar f Text
  , _userEmail      :: Columnar f Text
  , _userAge        :: Columnar f Int
  , _userOccupation :: Columnar f Text
  } deriving (Generic)

User 
  (LensFor userId) 
  (LensFor userName) 
  (LensFor userEmail) 
  (LensFor userAge) 
  (LensFor userOccupation) = tableLenses

type User = UserT Identity
type UserId = PrimaryKey UserT Identity

deriving instance Show User
deriving instance Eq User
deriving instance Show UserId
deriving instance Eq UserId

instance Beamable UserT
instance Beamable (PrimaryKey UserT)
instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Int64) deriving Generic
  primaryKey = UserId . _userId

data ArticleT f = Article
  { _articleId            :: Columnar f Int
  , _articleTitle         :: Columnar f Text
  , _articleBody          :: Columnar f Text
  , _articlePublishedTime :: Columnar f Int64
  , _articleUserId        :: PrimaryKey UserT f
  } deriving (Generic)

Article
  (LensFor articleId)
  (LensFor articleTitle)
  (LensFor articleBody)
  (LensFor articlePublishedTime)
  (UserId (LensFor articleUserId)) = tableLenses

type Article = ArticleT Identity
type ArticleId = PrimaryKey ArticleT Identity

deriving instance Show Article
deriving instance Eq Article

instance Beamable ArticleT
instance Beamable (PrimaryKey ArticleT)
instance Table ArticleT where
  data PrimaryKey ArticleT f = ArticleId (Columnar f Int) deriving Generic
  primaryKey = ArticleId . _articleId

data BlogDB f = BlogDB
  { _blogUsers    :: f (TableEntity UserT)
  , _blogArticles :: f (TableEntity ArticleT)
  } deriving (Generic)

instance Database be BlogDB

blogDb :: DatabaseSettings be BlogDB
blogDb = defaultDbSettings

users :: [User]
users = [ user1, user2 ]

user1 :: User
user1 = User 1 "James" "james@example.com" 25 "programmer"

user2 :: User
user2 = User 2 "Katie" "katie@example.com" 25 "engineer"

articles :: [Article]
articles = [ article1, article2, article3 ]

article1 :: Article
article1 = Article 1 "First article" "A great article" 1531193221 (pk user1)

article2 :: Article
article2 = Article 2 "Second article" "A better article" 1531199221 (pk user2)

article3 :: Article
article3 = Article 3 "Third article" "The best article" 1531200221 (pk user1)
