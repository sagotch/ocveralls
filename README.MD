# ocveralls

![travis status](https://travis-ci.org/sagotch/ocveralls.svg?branch=test)

Produce JSON for [coveralls.io](https://coveralls.io/) from
[bisect](http://bisect.x9c.fr/index.html) coverage data.

## Usage

    ocveralls bisect*.out > coveralls.json

Options:

    --prefix      Prefix to add in order to find source and cmp files.
    --repo_token  Use repo token instead of automatic CI detection.

An `ocveralls` usage example:

    BISECT_FILE=coverage ./foo.byte
    ocveralls --prefix _build coverage*.out > coveralls.json
    curl -F 'json_file=@coveralls.json' 'https://coveralls.io/api/v1/jobs'

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

On [Travis-ci](https://travis-ci.org/) when installing
`ocveralls` with opam, you may encouter `ocveralls: command not found`.
Instead of calling `ocveralls`, use ``"`opam config var bin`/ocveralls"``.
