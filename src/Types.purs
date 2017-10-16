module Types
  ( AppView(..)
  , State
  , RemoteData(..)
  , WebData
  ) where

import Models.Book (BookSearchResult)

import Prelude


data AppView
  = AppView_Search
  | AppView_BookDetail

derive instance eqAppView :: Eq AppView

type State =
  { currentView :: AppView
  , searchResults :: WebData (Array BookSearchResult)
  , wishlist :: Array Int
  }

data RemoteData e a
  = RemoteData_NotAsked
  | RemoteData_Loading
  | RemoteData_Success a
  | RemoteData_Failure e

type WebData a = RemoteData String a
