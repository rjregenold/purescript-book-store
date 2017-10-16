module Components.BookDetailView where

import Action as A
import Models.Book (Book(..))
import Types (RemoteData(..), State, WebData)

import Data.Maybe (fromMaybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude

data Query a 
  = HandleInput (WebData Book) a
  | GoBack a

data Message
  = Dispatch (A.ActionDSL (State -> State))

type ComponentState = 
  { book :: WebData Book
  }

component :: forall m. H.Component HH.HTML Query (WebData Book) Message m
component =
  H.component
    { initialState
    , render
    , eval
    , receiver: HE.input HandleInput
    }
  where

  initialState :: WebData Book -> ComponentState
  initialState book = { book: book }

  render :: ComponentState -> H.ComponentHTML Query
  render state =
    HH.div_
        [ HH.button
            [ HE.onClick (HE.input_ GoBack) ]
            [ HH.text "Go Back" ]
        , case state.book of
            RemoteData_NotAsked -> 
              HH.p_ [ HH.text "" ]
            RemoteData_Loading  -> 
              HH.p_ [ HH.text "Loading, please wait..." ]
            RemoteData_Success res -> 
              renderBook res
            RemoteData_Failure err -> 
              HH.p_ [ HH.text err ]
        ]

  renderBook (Book book) =
    HH.div_
        [ HH.img
            [ HP.src book.coverURL ]
        , HH.p_
            [ HH.text book.title ]
        , HH.p_
            [ HH.text (fromMaybe "" book.synopsis) ]
        , HH.p_
            [ HH.text book.price ]
        ]

  eval :: Query ~> H.ComponentDSL ComponentState Query Message m
  eval = case _ of
    HandleInput book next -> do
      H.modify (\st -> st { book = book })
      pure next
    GoBack next -> do
      H.raise (Dispatch A.previousView)
      pure next
