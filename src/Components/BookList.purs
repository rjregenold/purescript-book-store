module Components.BookList where

import Action as A
import Models.Book (BookSearchResult(..))
import Types (RemoteData(..), State, WebData)

import Data.Maybe (fromMaybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Prelude

data Query a 
  = HandleInput (WebData (Array BookSearchResult)) a

data Message
  = Dispatch (A.ActionDSL (State -> State))

type ComponentState = 
  { searchResults :: WebData (Array BookSearchResult)
  }

component :: forall m. H.Component HH.HTML Query (WebData (Array BookSearchResult)) Message m
component =
  H.component
    { initialState
    , render
    , eval
    , receiver: HE.input HandleInput
    }
  where

  initialState :: (WebData (Array BookSearchResult)) -> ComponentState
  initialState st = { searchResults: st }

  render :: ComponentState -> H.ComponentHTML Query
  render state =
    HH.div_
      case state.searchResults of
        RemoteData_NotAsked -> [ HH.text "" ]
        RemoteData_Loading -> [ HH.text "Loading, please wait..." ]
        RemoteData_Success res -> [ HH.ul_ (map renderResult res) ]
        RemoteData_Failure err -> [ HH.text err ]

  renderResult (BookSearchResult res) =
    HH.li_ 
        [ HH.h3_ [ HH.text res.title ]
        , HH.p_ [ HH.text (fromMaybe "" res.synopsis) ]
        ]

  eval :: Query ~> H.ComponentDSL ComponentState Query Message m
  eval = case _ of
    HandleInput xs next -> do
      H.modify (\st -> st { searchResults = xs })
      pure next
