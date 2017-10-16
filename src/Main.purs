module Main where

import UI as UI
import Store (initialState, runAction)
import Types (State)

import Control.Coroutine as CR
import Control.Coroutine.Aff as CRA
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import DOM (DOM)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax as AX
import Prelude
import Redox (dispatch)
import Redox.Store (REDOX, Store, getState, mkStore, subscribe)


type ComponentEff eff =
  ( redox :: REDOX
  , ajax :: AX.AJAX
  , dom :: DOM
  | eff
  )

storeProducer :: forall eff
               . Store State
              -> CR.Producer State (Aff (HA.HalogenEffects (ComponentEff eff))) Unit
storeProducer store = CRA.produce \emit ->
  void (subscribe store (\st -> emit (Left st)))

storeConsumer :: forall eff
               . (UI.Query ~> Aff (HA.HalogenEffects eff))
              -> CR.Consumer State (Aff (HA.HalogenEffects eff)) Unit
storeConsumer query = CR.consumer \st -> do
  query (H.action (UI.StoreUpdated st))
  pure Nothing

storeDispatcher :: forall eff
                 . Store State
                -> CR.Consumer UI.Message (Aff (HA.HalogenEffects (ComponentEff eff))) Unit
storeDispatcher store = CR.consumer \msg -> do
  _ <- case msg of
    UI.Dispatch cmds -> do
      H.liftEff (dispatch (const (pure unit)) (runAction store) store cmds)
  pure Nothing

main :: forall eff. Eff (HA.HalogenEffects (ComponentEff eff)) Unit
main = HA.runHalogenAff do
  store <- mkStore initialState
  body <- HA.awaitBody
  initialState' <- getState store
  io <- runUI UI.component initialState' body
  io.subscribe (storeDispatcher store)
  CR.runProcess (storeProducer store CR.$$ storeConsumer io.query)
