module Component where

import Action as A
import Components.SearchView as SearchView
import Types (State)

import Control.Monad.Aff (Aff)
import Data.Either.Nested (Either1)
import Data.Functor.Coproduct.Nested (Coproduct1)
import Data.Maybe (Maybe(..))
import DOM (DOM)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Prelude

data Query a 
  = StoreUpdated State a
  | HandleSearchView SearchView.Message a

data Message
  = Dispatch (A.ActionDSL (State -> State))

type ChildQuery = Coproduct1 SearchView.Query

type ChildSlot = Either1 Unit

type ComponentEff eff =
  ( dom :: DOM
  | eff
  )

component :: forall eff. H.Component HH.HTML Query State Message (Aff (ComponentEff eff))
component =
  H.parentComponent
    { initialState: id
    , render
    , eval
    , receiver: const Nothing
    }
  where

  cpSearchView = CP.cp1

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot (Aff (ComponentEff eff))
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text "Book Store" ]
      , HH.slot' cpSearchView unit SearchView.component state (HE.input HandleSearchView)
      ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message (Aff (ComponentEff eff))
  eval = case _ of
    StoreUpdated st next -> do
      H.put st
      pure next
    HandleSearchView msg next -> do
      case msg of
        SearchView.Dispatch cmds -> H.raise (Dispatch cmds)
      pure next
