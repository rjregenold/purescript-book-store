module Components.BookList where

import Action as A
import Types (RemoteData(..), State, WebData)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Prelude

data Query a 
  = HandleInput (WebData String) a

data Message
  = Dispatch (A.ActionDSL (State -> State))

type ComponentState = 
  { searchResults :: WebData String
  }

component :: forall m. H.Component HH.HTML Query (WebData String) Message m
component =
  H.component
    { initialState
    , render
    , eval
    , receiver: HE.input HandleInput
    }
  where

  initialState :: (WebData String) -> ComponentState
  initialState st = { searchResults: st }

  render :: ComponentState -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.p_ 
          [ HH.text $ case state.searchResults of
              RemoteData_NotAsked -> ""
              RemoteData_Loading -> "Loading, please wait..."
              RemoteData_Success res -> res 
              RemoteData_Failure err -> err
          ]
      ]

  eval :: Query ~> H.ComponentDSL ComponentState Query Message m
  eval = case _ of
    HandleInput xs next -> do
      H.modify (\st -> st { searchResults = xs })
      pure next
