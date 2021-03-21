{ name = "purescript-veither"
, dependencies =
  [ "control"
  , "enums"
  , "heterogenous"
  , "invariant"
  , "maybe"
  , "psci-support"
  , "quickcheck"
  , "quickcheck-laws"
  , "spec"
  , "transformers"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
