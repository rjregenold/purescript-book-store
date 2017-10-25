module Components.BookDetailView where

import Action as A
import HTML (stripTags)
import Models.Book (Book(..))
import Types (RemoteData(..), State, WebData)

import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude

data Query a 
  = HandleInput ComponentState a
  | GoBack a
  | AddWishlist Book a
  | RemoveWishlist Book a

data Message
  = Dispatch (A.ActionDSL (State -> State))

type ComponentState = 
  { book :: WebData Book
  , wishlist :: Set Book
  }

component :: forall m. H.Component HH.HTML Query ComponentState Message m
component =
  H.component
    { initialState: id
    , render
    , eval
    , receiver: HE.input HandleInput
    }
  where

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
              renderBook state res
            RemoteData_Failure err -> 
              HH.p_ [ HH.text err ]
        ]

  renderBook state b@(Book book) =
    HH.div_
        [ HH.img
            [ HP.src book.coverURL ]
        , HH.p_
            [ HH.text book.title ]
        , HH.p_
            [ HH.text (fromMaybe "" (map stripTags book.synopsis)) ]
        , HH.p_
            [ HH.text book.price ]
        , if Set.member b state.wishlist
          then renderRemoveWishlistBtn b
          else renderAddWishlistBtn b
        ]

  renderAddWishlistBtn book =
    HH.button
        [ HE.onClick (HE.input_ (AddWishlist book)) ]
        [ HH.text "Add to wishlist" ]

  renderRemoveWishlistBtn book =
    HH.button
        [ HE.onClick (HE.input_ (RemoveWishlist book)) ]
        [ HH.text "Remove from wishlist" ]

  eval :: Query ~> H.ComponentDSL ComponentState Query Message m
  eval = case _ of
    HandleInput st next -> do
      H.put st
      pure next
    GoBack next -> do
      H.raise (Dispatch A.previousView)
      pure next
    AddWishlist book next -> do
      H.raise (Dispatch (A.addWishlist book))
      pure next
    RemoveWishlist book next -> do
      H.raise (Dispatch (A.removeWishlist book))
      pure next
