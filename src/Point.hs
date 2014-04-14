{-# LANGUAGE DeriveGeneric #-}

module Point where


import Data.Aeson
import qualified Data.Text as Text

import qualified Data.List as List

import GHC.Generics

data Colour = Red | Green deriving (Show, Eq, Generic)

instance FromJSON Colour
instance ToJSON Colour

data Point = Point { x :: Int, y :: Int, colour :: Colour }
             deriving (Show, Generic)

instance FromJSON Point
instance ToJSON Point


