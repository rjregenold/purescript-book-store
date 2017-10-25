module Components.BookList where

import Action as A
import HTML (stripTags)
import Models.Book (BookId, BookSearchResult(..))
import Types (AppView(..), RemoteData(..), State, WebData)

import Data.Maybe (fromMaybe)
import Data.String (length, take)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Prelude

data Query a 
  = HandleInput (WebData (Array BookSearchResult)) a
  | ShowBookDetail BookId a

data Message
  = Dispatch (A.ActionDSL (State -> State))

type ComponentState = 
  { searchResults :: WebData (Array BookSearchResult)
  }

component :: forall m. H.Component HH.HTML Query (WebData (Array BookSearchResult)) Message m
component =
  H.component
    { initialState
    , render
    , eval
    , receiver: HE.input HandleInput
    }
  where

  initialState :: (WebData (Array BookSearchResult)) -> ComponentState
  initialState st = { searchResults: st }

  render :: ComponentState -> H.ComponentHTML Query
  render state =
    HH.div_
      case state.searchResults of
        RemoteData_NotAsked -> [ HH.text "" ]
        RemoteData_Loading -> [ HH.text "Loading, please wait..." ]
        RemoteData_Success res -> [ HH.ul_ (map renderResult res) ]
        RemoteData_Failure err -> [ HH.text err ]

  renderResult (BookSearchResult res) =
    HH.li
        [ HE.onClick (HE.input_ (ShowBookDetail res.bookId)) ]
        [ HH.h3_ [ HH.text res.title ]
        , HH.p_ [ HH.text res.price ]
        , HH.p_ [ HH.text (renderSynopsis res.synopsis) ]
        ]

  renderSynopsis =
    fromMaybe "" <<< map (truncateSynopsis <<< stripTags)

  truncateLength = 100

  truncateSynopsis s =
    if length s <= truncateLength
    then s
    else (take truncateLength s) <> "..."


  eval :: Query ~> H.ComponentDSL ComponentState Query Message m
  eval = case _ of
    HandleInput xs next -> do
      H.modify (\st -> st { searchResults = xs })
      pure next
    ShowBookDetail bookId next -> do
      H.raise (Dispatch $ do
        _ <- A.bookDetails bookId
        A.changeView AppView_BookDetail)
      pure next
