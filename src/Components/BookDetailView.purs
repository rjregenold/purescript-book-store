module Components.BookDetailView where

import Action as A
import Types (State)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Prelude

data Query a 
  = HandleInput Int a

data Message
  = Dispatch (A.ActionDSL (State -> State))

type ComponentState = 
  { bookId :: Int
  }

component :: forall m. H.Component HH.HTML Query Int Message m
component =
  H.component
    { initialState
    , render
    , eval
    , receiver: HE.input HandleInput
    }
  where

  initialState :: Int -> ComponentState
  initialState st = { bookId: st }

  render :: ComponentState -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.p_ 
          [ HH.text (show state.bookId) ]
      ]

  eval :: Query ~> H.ComponentDSL ComponentState Query Message m
  eval = case _ of
    HandleInput bookId next -> do
      H.modify (\st -> st { bookId = bookId })
      pure next
