module Test.Main where

import Test.HTMLTest as HTML
import Test.Models.BookTest as Book

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Prelude
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)


main :: forall eff. Eff (console ∷ CONSOLE, testOutput ∷ TESTOUTPUT, avar ∷ AVAR | eff) Unit
main = runTest do
  Book.bookTest
  HTML.htmlTest
