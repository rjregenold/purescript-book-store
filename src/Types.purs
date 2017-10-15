module Types
  ( State
  , RemoteData(..)
  , WebData
  ) where

import Data.Maybe (Maybe)


type State =
  { query :: Maybe String
  , searchResults :: WebData String
  , wishlist :: Array Int
  }

data RemoteData e a
  = RemoteData_NotAsked
  | RemoteData_Loading
  | RemoteData_Success a
  | RemoteData_Failure e

type WebData a = RemoteData String a
