{ name = "env-vars"
, dependencies =
  [ "either"
  , "foreign-object"
  , "integers"
  , "argonaut"
  , "simple-text"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
