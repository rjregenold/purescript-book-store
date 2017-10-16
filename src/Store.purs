module Store where

import Action (Action(..), ActionDSL)
import Types (AppView(..), RemoteData(..), State)

import Control.Comonad.Cofree (Cofree, exploreM, unfoldCofree)
import Control.Monad.Aff (Aff)
import Data.Argonaut.Decode (decodeJson)
import Data.Either (Either(..))
import Network.HTTP.Affjax as AX
import Prelude
import Redox (Store, mkIncInterp, runSubscriptions)


newtype Run eff a = Run
  { changeView :: AppView -> Aff eff a
  , search :: String -> Aff eff a
  , searchLoading :: Aff eff a
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
  , searchResults: RemoteData_NotAsked
  , wishlist: []
  }

type Interp eff a = Cofree (Run eff) a

mkInterp :: forall eff. State -> Interp (StoreEff eff) State
mkInterp state = unfoldCofree id next state
  where
        search query = do
          res <- AX.get ("https://bookshout.com/api/books/search.json?query=" <> query)
          case decodeJson res.response of
           Right res' ->
             pure (state { searchResults = RemoteData_Success res' })
           Left err ->
             pure (state { searchResults = RemoteData_Failure err })

        next :: State -> Run (StoreEff eff) State
        next state' = Run
          { changeView: \nextView -> pure (state' { currentView = nextView })
          , search: search
          , searchLoading: pure (state' { searchResults = RemoteData_Loading })
          , addWishlist: \id' -> pure state'
          , removeWishlist: \id' -> pure state'
          }

pair :: forall x y eff. Action (x -> y) -> Run (StoreEff eff) x -> Aff (StoreEff eff) y
pair (ChangeView nextView f) (Run interp) = map f (interp.changeView nextView)
pair (Search query f) (Run interp) = map f (interp.search query)
pair (SearchLoading f) (Run interp) = map f interp.searchLoading
pair (AddWishlist id' f) (Run interp) = map f (interp.addWishlist id')
pair (RemoveWishlist id' f) (Run interp) = map f (interp.removeWishlist id')

runAction :: forall eff. Store State -> ActionDSL (State -> State) -> State -> Aff (StoreEff eff) State
runAction store cmds state = exploreM pair cmds ((runSubscriptions store <<< mkIncInterp store <<< mkInterp) state)
