module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpecT)
import Test.Data.Veither as V

main :: Effect Unit
main = launchAff_ $ runSpecT defaultConfig [ consoleReporter ] do
  V.spec
