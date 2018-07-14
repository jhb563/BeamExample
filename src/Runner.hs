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
{-# LANGUAGE NoMonomorphismRestriction #-}

module Runner where

import Control.Lens ((^.))
import Database.SQLite.Simple
import Database.Beam
import Database.Beam.Sqlite

import Schema

main :: IO ()
main = do
  conn <- open "blogdb1.db"
  insertUsers conn

users' = [ user1' ]

user1' = User
  default_ 
  (val_ "James") 
  (val_ "james@example.com") 
  (val_ 25) 
  (val_ "programmer")

insertUsers :: Connection -> IO ()
insertUsers conn = runBeamSqlite conn $ runInsert $ 
  insert (_blogUsers blogDb) $ insertExpressions users'

insertArticles :: Connection -> IO ()
insertArticles conn = runBeamSqlite conn $ runInsert $ 
  insert (_blogArticles blogDb) $ insertValues articles

findUsers :: Connection -> IO ()
findUsers conn = runBeamSqlite conn $ do
  users <- runSelectReturningList $ select $ do
    user <- (all_ (_blogUsers blogDb))
    article <- (all_ (_blogArticles blogDb))
    guard_ (user ^. userName ==. (val_ "James"))
    guard_ (article ^. articleUserId ==. user ^. userId) 
    return (user, article)
  mapM_ (liftIO . putStrLn . show) users
