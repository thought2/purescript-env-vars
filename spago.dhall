{ name = "env-vars"
, dependencies =
  [ "either"
  , "foreign-object"
  , "integers"
  , "node-process"
  , "simple-text"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
