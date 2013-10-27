#! /bin/sh

cabal run -v0 -- -a tests/archetypes/any.archetype         tests/inputs/*.json            && \
cabal run -v0 -- -a tests/archetypes/simple.archetype      tests/inputs/simple*.json      && \
cabal run -v0 -- -a tests/archetypes/generic.archetype     tests/inputs/generic*.json     && \
cabal run -v0 -- -a tests/archetypes/re.archetype          tests/inputs/re*.json          && \
cabal run -v0 -- -a tests/archetypes/assignments.archetype tests/inputs/assignments*.json && \
cabal run -v0 -- -a tests/archetypes/numbers.archetype     tests/inputs/numbers*.json     && \
cabal run -v0 -- -a tests/archetypes/quantifiers.archetype tests/inputs/quantifiers*.json

