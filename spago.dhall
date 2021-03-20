{ name = "purescript-veither"
, dependencies =
  [ "control"
  , "enums"
  , "invariant"
  , "maybe"
  , "psci-support"
  , "quickcheck"
  , "spec"
  , "transformers"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
