# ocveralls

![travis status](https://travis-ci.org/sagotch/ocveralls.ml.svg?branch=test)

Produce JSON for [coveralls.io](https://coveralls.io/) from
[bisect](http://bisect.x9c.fr/index.html) coverage data.

## Usage

    ocveralls bisect*.out > coveralls.json

Options:
    --prefix      Prefix to add in order to find source and cmp files.
    --repo_token  Use repo token instead of automatic CI detection.

See also `Makefile` of `test` branch for a complete example.

## Installation

From sources. Set `bindir` to the directory where to install the binary:
    make build
	make bindir=/foo/bar install

Via opam:
    opam install ocveralls

## Know bug(s)

Running the tool will generate a `bisectXXXX.out` file
(XXXX being a four digit number). This is due to a side effect of
`Bisect.Common` module. If this file really is a problem, define the
`BISECT_FILE` environment variable to a temporary directory, or
to a directory you do not have writing rights (the second solution
will generate a warning message on stderr, except if you also
define `BISECT_SILENT` to `YES` or `ON`).
