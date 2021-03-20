{ name = "purescript-veither"
, dependencies =
  [ "control"
  , "enums"
  , "invariant"
  , "maybe"
  , "psci-support"
  , "transformers"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
