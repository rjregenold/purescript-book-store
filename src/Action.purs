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

import Models.Book (BookId)
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
  | AddWishlist Int a
  | RemoveWishlist Int a

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

addWishlist :: Int -> ActionDSL (State -> State)
addWishlist id' = liftF (AddWishlist id' id)

removeWishlist :: Int -> ActionDSL (State -> State)
removeWishlist id' = liftF (RemoveWishlist id' id)
