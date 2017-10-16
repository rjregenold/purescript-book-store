module Components.SearchView where

import Action as A
import Components.BookList as BookList
import Types (State)

import Control.Monad.Aff (Aff)
import Data.Either.Nested (Either1)
import Data.Functor.Coproduct.Nested (Coproduct1)
import Data.Maybe (Maybe(..))
import DOM (DOM)
import DOM.Classy.Event (toEvent, preventDefault)
import DOM.Event.Event (Event)
import DOM.HTML.Indexed.ButtonType (ButtonType(..))
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude

data Query a 
  = HandleInput State a
  | SetQuery String a
  | PreventDefault Event (Query a)
  | RequestSearch a
  | HandleBookList BookList.Message a

data Message
  = Dispatch (A.ActionDSL (State -> State))

type ComponentState = 
  { state :: State
  , query :: String
  }

type ChildQuery = Coproduct1 BookList.Query

type ChildSlot = Either1 Unit

type ComponentEff eff =
  ( dom :: DOM
  | eff
  )

noDefaultSubmit :: forall a. (Unit -> Query Unit) -> HP.IProp (onSubmit :: Event | a) (Query Unit)
noDefaultSubmit q =
  HE.onSubmit (\e -> Just (PreventDefault (toEvent e) (H.action q)))

component :: forall eff. H.Component HH.HTML Query State Message (Aff (ComponentEff eff))
component =
  H.parentComponent
    { initialState
    , render
    , eval
    , receiver: HE.input HandleInput
    }
  where

  cpBookList = CP.cp1

  initialState :: State -> ComponentState
  initialState st = { state: st, query: "" }

  render :: ComponentState -> H.ParentHTML Query ChildQuery ChildSlot (Aff (ComponentEff eff))
  render state =
    HH.div_
      [ HH.form
          [ noDefaultSubmit RequestSearch ]
          [ HH.input
              [ HE.onValueInput (HE.input SetQuery) 
              , HP.placeholder "Find your next book"
              ]
          , HH.button
              [ HP.type_ ButtonSubmit ]
              [ HH.text "Search" ]
          ]
      , HH.slot' cpBookList unit BookList.component state.state.searchResults (HE.input HandleBookList)
      ]

  eval :: Query ~> H.ParentDSL ComponentState Query ChildQuery ChildSlot Message (Aff (ComponentEff eff))
  eval = case _ of
    HandleInput st next -> do
      H.modify (\state -> state { state = st })
      pure next
    SetQuery query next -> do
      H.modify (\st -> st { query = query })
      pure next
    PreventDefault ev q -> do
      H.liftEff (preventDefault ev)
      eval q
    RequestSearch next -> do
      st <- H.get
      H.raise (Dispatch (A.search st.query))
      pure next
    HandleBookList msg next -> do
      case msg of
        BookList.Dispatch cmds -> H.raise (Dispatch cmds)
      pure next
