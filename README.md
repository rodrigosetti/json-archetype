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

    /**
     * JSON archetype language is a superset of JSON, and can accept C-style
     * comments (block or line) to help documenting your test.
     */

    /* one case assign sub-structes to names, and use them later in the main
       validation structure to reuse parts. This is also an example of a
       non-trivial use of a regular expression to validate an IP address. */
    ip_address =  "[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}"
    server = { "ip": ip_address,
               "port": 8080,
               "protocol": "http"}

    {
        "name": string,
        "version": 2,
        "data(-.*)?": object,  // this key is a regular expression
        "is_member" : boolean, // true or false (type restriction)
        "is_active" : true,    // only true (value restriction)
        "coordinates": ["geo", number, number, number], // any array with "geo" and 3 numbers
        "stream": array,

        // reusing pre-defined structures...
        "prod_server" : server,
        "stage_server": server,
        "dev_server"  : server,

        "extra": any // accepts any value here. the key "extra" must exist.
    }

It defines a structure a JSON must have in order to be valid. The JSON can be
validated with an exact value (_e.g._ "http"), or with a type constrain such as
`string` or `number`, or even regular expressions (in string literals or object
key identifiers). Actually every string and key matching is a regular
expression matching, so please be mindful of special characters and proper
escaping.

For the given archetype, the following JSON is an example of a valid one (note
that order matters in arrays, but it doesn't in objects):

    {
        "version": 2,
        "is_member" : false,
        "name": "John Smith",
        "is_active" : true,
        "stream": [7, 7, 7, 10, 22, 12, 2, 8],
        "coordinates": ["geo", 3123, 133, 319],

        "dev_server"   : {"ip": "127.0.0.1", "port": 8080, "protocol": "http"},
        "stage_server" : {"ip": "200.6.5.1", "port": 8080, "protocol": "http"},
        "prod_server"  : {"ip": "192.178.1.1", "port": 8080, "protocol": "http"},

        "data-color": {"red": 10, "green": 20, "blue": 6},
        "extra": {"foo": "bar",
                  "bar": null}
    }


## TODO

 * Use optional postfix quantity qualifiers (`*`, `+`, `?`, `{n}`, `{n-m}`), to
   represent items that can appear optionally, zero or more, one or more,
   _etc._

