module Test.Models.BookTest where

import Models.Book (BookId(..), BookSearchResult(..))

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Prelude
import Test.Unit (TestSuite, failure, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)


bookTest :: forall eff. TestSuite eff
bookTest = 
  let js0 = "{\"id\":1234567, \"title\":\"Book Title\", \"current_price\":\"$0.99\", \"synopsis\": \"This is a description.\"}"
      js1 = "{\"id\":1234567, \"title\":\"Book Title\", \"current_price\":\"$0.99\", \"synopsis\": null}"
  in do
  suite "BookSearchResult DecodeJson" do
    test "decode search result" do
      runDecodeTest js0 $ \x -> do
        Assert.assert "should decode id" (x.bookId == BookId 1234567)
        Assert.assert "should decode title" (x.title == "Book Title")
        Assert.assert "should decode price" (x.price == "$0.99")
        Assert.assert "should decode missing synopsis" (x.synopsis == Just "This is a description.")
    test "decode search result with missing synopsis" do
      runDecodeTest js1 $ \x -> do
        Assert.assert "should decode id" (x.bookId == BookId 1234567)
        Assert.assert "should decode title" (x.title == "Book Title")
        Assert.assert "should decode price" (x.price == "$0.99")
        Assert.assert "should decode missing synopsis" (x.synopsis == Nothing)
    where
      runDecodeTest js f =
        either
          failure
          (\(BookSearchResult x) -> f x)
          (jsonParser js >>= decodeJson)

main :: forall eff. Eff (console ∷ CONSOLE, testOutput ∷ TESTOUTPUT, avar ∷ AVAR | eff) Unit
main = runTest bookTest
