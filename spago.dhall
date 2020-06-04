{ name = "env-vars"
, dependencies =
  [ "argonaut"
  , "either"
  , "foreign-object"
  , "integers"
  , "record-extra"
  , "simple-text"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
