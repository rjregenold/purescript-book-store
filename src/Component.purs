module Component where

import Action as A
import Types (RemoteData(..), State)

import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..))
import DOM (DOM)
import DOM.Classy.Event (toEvent, preventDefault)
import DOM.Event.Event (Event)
import DOM.HTML.Indexed.ButtonType (ButtonType(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude

data Query a 
  = SetQuery String a
  | PreventDefault Event (Query a)
  | RequestSearch a
  | StoreUpdated State a

data Message
  = Dispatch (A.ActionDSL (State -> State))

type ComponentState = 
  { state :: State
  , query :: String
  }

type ComponentEff eff =
  ( dom :: DOM
  | eff
  )

noDefaultSubmit :: forall a. (Unit -> Query Unit) -> HP.IProp (onSubmit :: Event | a) (Query Unit)
noDefaultSubmit q =
  HE.onSubmit (\e -> Just (PreventDefault (toEvent e) (H.action q)))

component :: forall eff. H.Component HH.HTML Query State Message (Aff (ComponentEff eff))
component =
  H.component
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State -> ComponentState
  initialState st = { state: st, query: "" }

  render :: ComponentState -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text "Book Store" ]
      , HH.form
          [ noDefaultSubmit RequestSearch ]
          [ HH.input
              [ HE.onValueInput (HE.input SetQuery) ]
          , HH.button
              [ HP.type_ ButtonSubmit ]
              [ HH.text "Search" ]
          ]
      , HH.p_ 
          [ HH.text $ case state.state.searchResults of
              RemoteData_NotAsked -> ""
              RemoteData_Loading -> "Loading, please wait..."
              RemoteData_Success res -> res 
              RemoteData_Failure err -> err
          ]
      ]

  eval :: Query ~> H.ComponentDSL ComponentState Query Message (Aff (ComponentEff eff))
  eval = case _ of
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
    StoreUpdated st next -> do
      H.modify (\state -> state { state = st })
      pure next
