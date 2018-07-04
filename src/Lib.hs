module Lib
    ( someFunc
    ) where

import Database.Beam
import Database.Beam.Postgres

someFunc :: IO ()
someFunc = putStrLn "someFunc"
