module Action 
  ( Action(..)
  , ActionDSL
  , changeView
  , previousView
  , search
  , bookDetails
  , addWishlist
  , removeWishlist
  ) where

import Models.Book (Book, BookId)
import Types (AppView, State)

import Control.Monad.Free (Free, liftF)
import Prelude


data Action a
  = ChangeView AppView a
  | PreviousView a
  | SearchLoading a
  | Search String a
  | BookDetailsLoading a
  | BookDetails BookId a
  | AddWishlist Book a
  | RemoveWishlist Book a

derive instance functorAction :: Functor Action

type ActionDSL a = Free Action a

changeView :: AppView -> ActionDSL (State -> State)
changeView nextView = liftF (ChangeView nextView id)

previousView :: ActionDSL (State -> State)
previousView = liftF (PreviousView id)

searchLoading :: ActionDSL (State -> State)
searchLoading = liftF (SearchLoading id)

search :: String -> ActionDSL (State -> State)
search query = do
  _ <- searchLoading
  liftF (Search query id)

bookDetailsLoading :: ActionDSL (State -> State)
bookDetailsLoading = liftF (BookDetailsLoading id)

bookDetails :: BookId -> ActionDSL (State -> State)
bookDetails bookId = do
  _ <- bookDetailsLoading
  liftF (BookDetails bookId id)

addWishlist :: Book -> ActionDSL (State -> State)
addWishlist book = liftF (AddWishlist book id)

removeWishlist :: Book -> ActionDSL (State -> State)
removeWishlist book = liftF (RemoveWishlist book id)
