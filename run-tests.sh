#! /bin/sh

runhaskell src/Main.hs -a tests/archetypes/any.archetype tests/inputs/*.json
runhaskell src/Main.hs -a tests/archetypes/simple.archetype tests/inputs/simple-*.json
runhaskell src/Main.hs -a tests/archetypes/generic.archetype tests/inputs/generic-*.json
runhaskell src/Main.hs -a tests/archetypes/re.archetype tests/inputs/re-*.json

