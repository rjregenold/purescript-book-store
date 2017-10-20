module Store where

import Action (Action(..), ActionDSL)
import Models.Book (Book, BookId(BookId))
import Types (AppView(..), RemoteData(..), State)

import Control.Comonad.Cofree (Cofree, exploreM, unfoldCofree)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Argonaut.Decode (decodeJson)
import Data.Array (snoc, unsnoc)
import Data.Either (either)
import Data.Generic (class Generic)
import Data.Maybe (maybe)
import Data.Set as Set
import DOM (DOM)
import DOM.WebStorage (STORAGE, getItem, setItem, getLocalStorage)
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
  , addWishlist :: Book -> Aff eff a
  , removeWishlist :: Book -> Aff eff a
  }

derive instance functorRun :: Functor (Run eff)

type StoreEff eff =
  ( ajax :: AX.AJAX
  , dom :: DOM
  , storage :: STORAGE
  | eff
  )

newtype UserWishlist = UserWishlist { wishlist :: Array Book }

derive instance genericUserWishlist :: Generic UserWishlist

data StorageKey a
  = UserWishlistKey

derive instance genericStorageKey :: Generic (StorageKey a)

userWishlistKey :: StorageKey UserWishlist
userWishlistKey = UserWishlistKey

initialState :: forall eff. Eff (dom :: DOM, storage :: STORAGE | eff) State
initialState = do
  localStorage <- getLocalStorage
  result <- getItem localStorage userWishlistKey
  pure { currentView: AppView_Search
       , viewStack: []
       , searchResults: RemoteData_NotAsked
       , bookDetails: RemoteData_NotAsked
       , wishlist: maybe Set.empty (\(UserWishlist x) -> Set.fromFoldable x.wishlist) result
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

    searchLoading state =
      pure (state { searchResults = RemoteData_Loading })

    search state query = do
      res <- AX.get ("https://bookshout.com/api/books/search.json?query=" <> query)
      let x = either RemoteData_Failure RemoteData_Success (decodeJson res.response)
      pure (state { searchResults = x })

    bookDetailsLoading state =
      pure (state { bookDetails = RemoteData_Loading })

    bookDetails state (BookId bookId) = do
      res <- AX.get ("https://bookshout.com/api/books/" <> show bookId <> ".json")
      let x = either RemoteData_Failure RemoteData_Success (decodeJson res.response)
      pure (state { bookDetails = x })

    persistWishlist wishlist = do
      localStorage <- getLocalStorage
      setItem localStorage userWishlistKey (UserWishlist { wishlist: Set.toUnfoldable wishlist })

    updateWishlist f state book = do
      let wishlist = f book state.wishlist
      _ <- liftEff (persistWishlist wishlist)
      pure (state { wishlist = wishlist })

    addWishlist = updateWishlist Set.insert

    removeWishlist = updateWishlist Set.delete 

    next :: State -> Run (StoreEff eff) State
    next state = Run
      { changeView: changeView state
      , previousView: previousView state
      , searchLoading: searchLoading state
      , search: search state
      , bookDetailsLoading: bookDetailsLoading state
      , bookDetails: bookDetails state
      , addWishlist: addWishlist state
      , removeWishlist: removeWishlist state
      }

pair :: forall x y eff. Action (x -> y) -> Run (StoreEff eff) x -> Aff (StoreEff eff) y
pair (ChangeView nextView f) (Run interp) = map f (interp.changeView nextView)
pair (PreviousView f) (Run interp) = map f interp.previousView
pair (SearchLoading f) (Run interp) = map f interp.searchLoading
pair (Search query f) (Run interp) = map f (interp.search query)
pair (BookDetailsLoading f) (Run interp) = map f interp.bookDetailsLoading
pair (BookDetails bookId f) (Run interp) = map f (interp.bookDetails bookId)
pair (AddWishlist book f) (Run interp) = map f (interp.addWishlist book)
pair (RemoveWishlist book f) (Run interp) = map f (interp.removeWishlist book)

runAction :: forall eff. Store State -> ActionDSL (State -> State) -> State -> Aff (StoreEff eff) State
runAction store cmds state = exploreM pair cmds ((runSubscriptions store <<< mkIncInterp store <<< mkInterp) state)
