module Store where

import Action (Action(..), ActionDSL)
import Models.Book (BookId(..))
import Types (AppView(..), RemoteData(..), State)

import Control.Comonad.Cofree (Cofree, exploreM, unfoldCofree)
import Control.Monad.Aff (Aff)
import Data.Argonaut.Decode (decodeJson)
import Data.Array (snoc, unsnoc)
import Data.Either (either)
import Data.Maybe (maybe)
import Network.HTTP.Affjax as AX
import Prelude
import Redox (Store, mkIncInterp, runSubscriptions)


newtype Run eff a = Run
  { changeView :: AppView -> Aff eff a
  , previousView :: Aff eff a
  , searchLoading :: Aff eff a
  , search :: String -> Aff eff a
  , bookDetailsLoading :: Aff eff a
  , bookDetails :: BookId -> Aff eff a
  , addWishlist :: Int -> Aff eff a
  , removeWishlist :: Int -> Aff eff a
  }

derive instance functorRun :: Functor (Run eff)

type StoreEff eff =
  ( ajax :: AX.AJAX
  | eff
  )

initialState :: State
initialState = 
  { currentView: AppView_Search
  , viewStack: []
  , searchResults: RemoteData_NotAsked
  , bookDetails: RemoteData_NotAsked
  , wishlist: []
  }

type Interp eff a = Cofree (Run eff) a

mkInterp :: forall eff. State -> Interp (StoreEff eff) State
mkInterp = unfoldCofree id next
  where
        changeView state nextView = pure $ state
          { currentView = nextView
          , viewStack = snoc state.viewStack state.currentView
          }

        previousView state =
          maybe
            (pure state)
            (\x -> pure $ state { currentView = x.last, viewStack = x.init })
            (unsnoc state.viewStack)

        search state query = do
          res <- AX.get ("https://bookshout.com/api/books/search.json?query=" <> query)
          let x = either RemoteData_Failure RemoteData_Success (decodeJson res.response)
          pure (state { searchResults = x })

        bookDetails state (BookId bookId) = do
          res <- AX.get ("https://bookshout.com/api/books/" <> show bookId <> ".json")
          let x = either RemoteData_Failure RemoteData_Success (decodeJson res.response)
          pure (state { bookDetails = x })

        next :: State -> Run (StoreEff eff) State
        next state = Run
          { changeView: changeView state
          , previousView: previousView state
          , searchLoading: pure $ state { searchResults = RemoteData_Loading }
          , search: search state
          , bookDetailsLoading: pure $ state { bookDetails = RemoteData_Loading }
          , bookDetails: bookDetails state
          , addWishlist: \id' -> pure state
          , removeWishlist: \id' -> pure state
          }

pair :: forall x y eff. Action (x -> y) -> Run (StoreEff eff) x -> Aff (StoreEff eff) y
pair (ChangeView nextView f) (Run interp) = map f (interp.changeView nextView)
pair (PreviousView f) (Run interp) = map f interp.previousView
pair (SearchLoading f) (Run interp) = map f interp.searchLoading
pair (Search query f) (Run interp) = map f (interp.search query)
pair (BookDetailsLoading f) (Run interp) = map f interp.bookDetailsLoading
pair (BookDetails bookId f) (Run interp) = map f (interp.bookDetails bookId)
pair (AddWishlist id' f) (Run interp) = map f (interp.addWishlist id')
pair (RemoveWishlist id' f) (Run interp) = map f (interp.removeWishlist id')

runAction :: forall eff. Store State -> ActionDSL (State -> State) -> State -> Aff (StoreEff eff) State
runAction store cmds state = exploreM pair cmds ((runSubscriptions store <<< mkIncInterp store <<< mkInterp) state)
