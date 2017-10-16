module UI where

import Action as A
import Components.BookDetailView as BookDetailView
import Components.SearchView as SearchView
import Types (AppView(..), State)

import Control.Monad.Aff (Aff)
import Data.Array (snoc)
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(..))
import DOM (DOM)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude

data Query a 
  = StoreUpdated State a
  | HandleBookDetailView BookDetailView.Message a
  | HandleSearchView SearchView.Message a

data Message
  = Dispatch (A.ActionDSL (State -> State))

type ChildQuery = Coproduct2 SearchView.Query BookDetailView.Query

type ChildSlot = Either2 Unit Unit

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
  cpBookDetailView = CP.cp2

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot (Aff (ComponentEff eff))
  render state =
    HH.div_
      [ HH.h1_ [ HH.text "Book Store" ]
      , HH.div
          [ viewClasses state.currentView AppView_Search ]
          [ HH.slot' cpSearchView unit SearchView.component state (HE.input HandleSearchView) ]
      , HH.div
          [ viewClasses state.currentView AppView_BookDetail ]
          [ HH.slot' cpBookDetailView unit BookDetailView.component (state.bookDetails) (HE.input HandleBookDetailView) ]
      ]

  viewClasses a b =
    HP.classes (condClass a b [ HH.ClassName "base-view" ] (HH.ClassName "active"))

  condClass :: forall a. Eq a => a -> a -> Array HH.ClassName -> HH.ClassName -> Array HH.ClassName
  condClass a b baseClasses activeClass =
    if a == b
    then snoc baseClasses activeClass
    else baseClasses

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message (Aff (ComponentEff eff))
  eval = case _ of
    StoreUpdated st next -> do
      H.put st
      pure next
    HandleSearchView msg next -> do
      case msg of
        SearchView.Dispatch cmds -> H.raise (Dispatch cmds)
      pure next
    HandleBookDetailView msg next -> do
      case msg of
        BookDetailView.Dispatch cmds -> H.raise (Dispatch cmds)
      pure next
