module Test.Data.Veither where

import Prelude

import Control.Monad.Gen (chooseInt)
import Data.Array (all)
import Data.Veither (Veither)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Test.QuickCheck (quickCheckGen)
import Test.Spec (SpecT, describe)

spec :: forall m. MonadEffect m => SpecT Aff Unit m Unit
spec = do
  describe "type class instance tests" do
    liftEffect $ quickCheckGen do
      i <- chooseInt bottom top
      e <- chooseInt bottom top
      let
        vi :: Veither (foo :: Int) Int
        vi = pure i

        ve :: Veither (foo :: Int) Int
        ve = pure e

      pure $ all identity
        [ vi == vi
        , ve == ve
        , vi /= ve
        , ve /= vi
        , (compare i e) == (compare vi ve)
        , (compare e i) == (compare ve vi)
        ]
