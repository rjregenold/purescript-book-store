module Test.HTMLTest where

import HTML (stripTags)

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


htmlTest :: forall eff. TestSuite eff
htmlTest = do
  suite "HTML stripTags" do
    test "handles empty string" do
      Assert.equal "" (stripTags "")

    test "handles string with no tags" do
      Assert.equal "this is a test" (stripTags "this is a test")

    test "handles string with simple tags" do
      Assert.equal "this is a test" (stripTags "<b>this</b> is a test")

    test "handles unbalanced tags" do
      Assert.equal "this is a test" (stripTags "<b><i>this is</i> a test")

    test "handles bad html" do
      Assert.equal "this is a test" (stripTags "this is</b> a test")

main :: forall eff. Eff (console ∷ CONSOLE, testOutput ∷ TESTOUTPUT, avar ∷ AVAR | eff) Unit
main = runTest htmlTest
