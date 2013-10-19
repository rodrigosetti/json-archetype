#! /bin/sh

runhaskell -Wall -isrc Main -a tests/archetypes/any.archetype         tests/inputs/*.json            && \
runhaskell       -isrc Main -a tests/archetypes/simple.archetype      tests/inputs/simple*.json      && \
runhaskell       -isrc Main -a tests/archetypes/generic.archetype     tests/inputs/generic*.json     && \
runhaskell       -isrc Main -a tests/archetypes/re.archetype          tests/inputs/re*.json          && \
runhaskell       -isrc Main -a tests/archetypes/assignments.archetype tests/inputs/assignments*.json && \
runhaskell       -isrc Main -a tests/archetypes/quantifiers.archetype tests/inputs/quantifiers*.json

