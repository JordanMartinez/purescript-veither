{ name = "purescript-veither"
, dependencies =
  [ "control"
  , "enums"
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
