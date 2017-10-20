module Models.Book where

import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.?))
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Prelude


newtype BookId = BookId Int

derive instance eqBookId :: Eq BookId
derive instance ordBookId :: Ord BookId
derive instance newtypeBookId :: Newtype BookId _

instance decodeBookId :: DecodeJson BookId where
  decodeJson = map BookId <<< decodeJson

newtype ISBN = ISBN String

derive instance eqISBN :: Eq ISBN
derive instance newtypeISBN :: Newtype ISBN _
derive instance ordISBN :: Ord ISBN

instance decodeJsonISBN :: DecodeJson ISBN where
  decodeJson = map ISBN <<< decodeJson

newtype BookSearchResult = BookSearchResult
  { bookId :: BookId
  , title :: String
  , price :: String
  , synopsis :: Maybe String
  }

derive instance newtypeBookSearchResult :: Newtype BookSearchResult _

instance decodeJsonBookSearchResult :: DecodeJson BookSearchResult where
  decodeJson json = do
    j <- decodeJson json
    bookId <- j .? "id"
    title <- j .? "title"
    price <- j .? "current_price"
    synopsis <- j .? "synopsis"
    pure (BookSearchResult { bookId, title, price, synopsis })

newtype Book = Book
  { bookId :: BookId
  , title :: String
  , isbn :: ISBN
  , coverURL :: String
  , price :: String
  , synopsis :: Maybe String
  }

derive instance newtypeBook :: Newtype Book _
derive instance eqBook :: Eq Book
derive instance ordBook :: Ord Book

instance decodeJsonBook :: DecodeJson Book where
  decodeJson json = do
    j <- decodeJson json
    bookId <- j .? "id"
    title <- j .? "title"
    isbn <- j .? "isbn"
    coverURL <- j .? "cover_url"
    price <- j .? "current_price"
    synopsis <- j .? "synopsis"
    pure (Book { bookId, title, isbn, coverURL, price, synopsis })
