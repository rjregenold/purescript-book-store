module Components.WishlistView where

import Action as A
import Models.Book (Book(..), ISBN(..), BookId)
import Types (AppView(AppView_BookDetail), State)

import Data.Array (fromFoldable)
import Data.Newtype (un)
import Data.Set (Set)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude

data Query a 
  = HandleInput ComponentState a
  | ShowBookDetail BookId a

data Message
  = Dispatch (A.ActionDSL (State -> State))

type ComponentState = 
  {  wishlist :: Set Book
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
    HH.ul_ (map renderBook (fromFoldable state.wishlist))

  renderBook (Book book) =
    HH.li
        [ HE.onClick (HE.input_ (ShowBookDetail book.bookId)) ]
        [ HH.img [ HP.src book.coverURL ]
        , HH.h3_ [ HH.text book.title ]
        , HH.p_ [ HH.text (un ISBN book.isbn) ]
        ]

  eval :: Query ~> H.ComponentDSL ComponentState Query Message m
  eval = case _ of
    HandleInput st next -> do
      H.put st
      pure next
    ShowBookDetail bookId next -> do
      H.raise (Dispatch $ do
        _ <- A.bookDetails bookId
        A.changeView AppView_BookDetail)
      pure next
