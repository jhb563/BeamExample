{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Schema where

import Database.Beam
import Database.Beam.Postgres

import Data.Time (UTCTime)
import Data.Text (Text)
import Data.UUID (UUID)

data UserT f = User
  { _userId         :: Columnar f UUID
  , _userName       :: Columnar f Text
  , _userEmail      :: Columnar f Text
  , _userAge        :: Columnar f Int
  , _userOccupation :: Columnar f Text
  } deriving (Generic)

type User = UserT Identity
type UserId = PrimaryKey UserT Identity

deriving instance Show User
deriving instance Eq User

instance Beamable UserT
instance Beamable (PrimaryKey UserT)
instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f UUID) deriving Generic
  primaryKey = UserId . _userId

-- TODO Add User ID
data ArticleT f = Article
  { _articleId            :: Columnar f UUID
  , _articleTitle         :: Columnar f Text
  , _articleBody          :: Columnar f Text
  , _articlePublishedTime :: Columnar f UTCTime
  } deriving (Generic)

type Article = ArticleT Identity
type ArticleId = PrimaryKey ArticleT Identity

deriving instance Show Article
deriving instance Eq Article

instance Beamable ArticleT
instance Beamable (PrimaryKey ArticleT)
instance Table ArticleT where
  data PrimaryKey ArticleT f = ArticleId (Columnar f UUID) deriving Generic
  primaryKey = ArticleId . _articleId

data BlogDB f = BlogDB
  { _blogUsers :: f (TableEntity UserT)
  , _blogArticles :: f (TableEntity ArticleT)
  } deriving (Generic)

-- instance Database be BlogDB
