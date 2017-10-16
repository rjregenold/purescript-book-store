module Action 
  ( Action(..)
  , ActionDSL
  , changeView
  , search
  , addWishlist
  , removeWishlist
  ) where

import Types (AppView, State)

import Control.Monad.Free (Free, liftF)
import Prelude


data Action a
  = ChangeView AppView a
  | Search String a
  | SearchLoading a
  | AddWishlist Int a
  | RemoveWishlist Int a

derive instance functorAction :: Functor Action

type ActionDSL a = Free Action a

changeView :: AppView -> ActionDSL (State -> State)
changeView nextView = liftF (ChangeView nextView id)

searchLoading :: ActionDSL (State -> State)
searchLoading = liftF (SearchLoading id)

search :: String -> ActionDSL (State -> State)
search query = do
  _ <- searchLoading
  liftF (Search query id)

addWishlist :: Int -> ActionDSL (State -> State)
addWishlist id' = liftF (AddWishlist id' id)

removeWishlist :: Int -> ActionDSL (State -> State)
removeWishlist id' = liftF (RemoveWishlist id' id)
