{ name = "silent-night"
, dependencies =
  [ "affjax"
  , "audio-behaviors"
  , "canvas"
  , "console"
  , "drawing"
  , "effect"
  , "psci-support"
  , "quickcheck"
  , "typelevel-klank-dev"
  , "web-touchevents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
