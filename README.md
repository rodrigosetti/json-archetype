# JSON Archetype

A JSON test helper tool. It compares the so called "JSON archetype", which is a
superset of JSON language, with one or more JSON files and output expected
structure.

The archetype is a specification of the expected JSON format. It can be an
strict definition of a JSON, but in a lot of cases it's desired to have some
generality, _e. g._ a "number" (with any value), or "boolean", or structured
values such as lists (with any values), _etc._

For these scenarios, the archetype is an augmented JSON where one can specify
exactly matching, type matching (through type keywords, such as "number",
"object", _etc._), quantity qualifiers (optional, zero or more, one or more,
_etc._), and regular expression (for strings and keys).

## Usage:

    json-test <options> [file [file ...]]
      -h           --help                Display help information
      -a FILENAME  --archetype=FILENAME  The JSON archetype file name

