# JSON Archetype

JSON Archetype is a language designed for testing JSON documents. It validates
an expected archetype agains one or more JSON documents.

This project can be thought of a "XSD" or "DTD" for JSON.

## Motivation

In several test scenarios (specially in API testing) one desires to compare the
"expected" JSON with the "actual" output from the code. By comparing the two
JSON structures the test can tell if they differ or not, but it's a rigid
approach since - without a supporting testing code - one cannot allow sensible
variations in the actual JSON (such as values that we don't care, arrays that
can have a varying number of elements, _etc._)

This language allows the definition of an "archetype", which is a JSON with the
addition of generic invariant specifications, such as quantity qualifiers,
regular expressions and type (not value) constrains.

Please read the example to have a sense of how this works.

## Example

Take, for instance, the following JSON archetype:

    /**
     * JSON archetype language is a superset of JSON, and can accept C-style
     * comments (block or line) to help documenting your test.
     */

    /* one can assign values to names, and use them later to reuse parts.
       This is also an example of a non-trivial use of a regular expression
       to validate an IP address. */
    ip_address =  "[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}"
    server = { "ip": ip_address,
               "port": 8080,
               "protocol": "http"}

    {
        "name": string,
        "version" ?: 2,          // this key is optional (?)
        "data(-.*)?" *: object,  // this key is a regular expression, and may
                                 // appear zero or more times
        "is_member"   : boolean, // true or false (type restriction)
        "is_active"   : true,    // only true (value restriction)
        "coordinates" : ["geo", number{3}], // any array with "geo" and 3 numbers
        "stream": [number*],     // a stream is an array of zero or more numbers

        // reusing pre-defined structures. This key should appear three times:
        "[a-z]+_server" {3}: server,

        "state" : [string{2,3}, number+], // 2-3 strings and one or more numbers

        "extra": any // accepts any value here. the key "extra" must exist.
    }

It defines a structure a JSON must have in order to be valid. The values can be
validated against an exact value (_e.g._ "http"), or with a type constrain such
as `string` or `number`, or even regular expressions (in string literals or
object key identifiers). Actually every string and key matching is a regular
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

        "dev_server"   : {"ip": "127.0.0.1",   "port": 8080, "protocol": "http"},
        "stage_server" : {"ip": "200.6.5.1",   "port": 8080, "protocol": "http"},
        "prod_server"  : {"ip": "192.178.1.1", "port": 8080, "protocol": "http"},

        "data-color": {"red": 10, "green": 20, "blue": 6},
        "data-font" : {"family": "Lucida", "size": 10, "unit": "pt"},

        "extra": {"foo": "bar",
                  "bar": null},
        "state": ["spam", "eggs", 1, 2, 3]
    }

## Syntax

On top of normal JSON syntax, one can use "type qualifiers", anywhere a JSON
value could be, to identify that a value must be of a specific type: `object`,
`list`, `string`, `number`, `boolean`, or `any` (`null` is a type and a value
itself).

Quantity qualifiers can be optionally used right after array values and object
key literals to specify that the given array value or object key/value pair
should occur a certain number of times. They have the same syntax as their
POSIX regular expressions counterparts: `?` (optional), `*` (zero or more), `+`
(one or more), `{n}` (n repetitions), or `{n,m}` (n to m repetitions).

## Installation and usage

To compile and install, please first install [Haskell](http://www.haskell.org)
for your platform.

Following that, run `cabal install` ([Cabal](http://www.haskell.org/cabal/) is
Haskell package manager, see `cabal help` for other options) to compile and
install the binary.

Finally, you can run the validator:

    json-test <options> [file [file ...]]
      -h           --help                Display help information
      -a FILENAME  --archetype=FILENAME  The JSON archetype file name

If no JSON filename is given, then reads from standard input.

