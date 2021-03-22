module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Data.Veither as V
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = do
  V.checkVeitherTypeClassLaws
  launchAff_ $ runSpec [ consoleReporter ] do
    V.spec
