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

If no JSON filename is given, then reads from standard input.

## Example

Take, for example, the following JSON archetype:

    {
        "name": string,
        "data(-.*)?": object,
        "version": 2,
        "coordinates": [number, number, number],
        "is_member": boolean,
        "is_active": true,
        "stream": array,
        "server": { "ip": "[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}",
                    "port": 8080,
                    "protocol": "http"},
        "extra": any
    }

It defines a structure a JSON must have in order to be valid. The JSON can be
validated with an exact value (_e.g._ "http"), or with a type constrain such as
`string` or `number`, or even regular expressions (in string literals or object
key identifiers). Actually every string and key matching is a regular
expression matching, so please be mindful of special characters and proper
escaping.

The order matters in arrays, but it doesn't in objects.

## TODO

 * Use optional postfix quantity qualifiers (`*`, `+`, `?`, `{n}`, `{n-m}`), to
   represent items that can appear optionally, zero or more, one or more,
   _etc._
 * Use assignment syntax (`identifier = value`) to reuse data.

