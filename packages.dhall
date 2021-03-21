let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.0-20210317/packages.dhall sha256:e2e744972f9b60188dcf07f41418661b505c9ee2e9f91e57e67daefad3a5ae09

in  upstream // { heterogenous = {
      dependencies = ["prelude", "record", "tuples", "functors", "variant", "either" ],
      repo = "https://github.com/natefaubion/purescript-heterogeneous.git",
      version = "v0.5.0"
}}
