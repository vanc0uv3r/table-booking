{-# LANGUAGE DeriveGeneric #-}

module DataType where



import Data.Time
import Data.Aeson
import GHC.Generics

data State =
  Role
      | ChooseDay
      | ChooseTime
      | EditForm
 


type Tables = [Table]
type Times = [UTCTime]
type Name = Maybe String
type Phone = Maybe String
type LastInput = Maybe String
type Persons = Maybe Int
type Interval = Integer
type Indexes = [Int]
type TablesNum = Integer
type DaysNum = Integer
type OpenTime = UTCTime
type CloseTime = UTCTime
type ToReturn = Bool

data Table = Table {time :: UTCTime,
                    isFree :: Bool,
                    name :: Maybe String,
                    phone :: Maybe String,
                    persons :: Maybe Int
} deriving (Show, Generic)



instance FromJSON Table
instance ToJSON Table
