#! /bin/sh

runhaskell -Wall src/Main.hs -a tests/archetypes/any.archetype         tests/inputs/*.json             && \
runhaskell       src/Main.hs -a tests/archetypes/simple.archetype      tests/inputs/simple-*.json      && \
runhaskell       src/Main.hs -a tests/archetypes/generic.archetype     tests/inputs/generic-*.json     && \
runhaskell       src/Main.hs -a tests/archetypes/re.archetype          tests/inputs/re-*.json          && \
runhaskell       src/Main.hs -a tests/archetypes/assignments.archetype tests/inputs/assignments-*.json

