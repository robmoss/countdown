# Countdown

An OCaml library for playing and solving word and maths puzzles as presented
on the classic BBC show
[Countdown](http://en.wikipedia.org/wiki/Countdown_%28game_show%29).

## License

This work is made available under the BSD 3-Clause license (see `LICENSE`).

## Requirements

This library requires OCaml >= 4.02 and a recent version of Findlib.

The OCaml compilers and Findlib can be installed using your system package
manager, using [OPAM](http://opam.ocaml.org/), using a platform-specific
third-party package manager (e.g., [GODI](http://godi.camlcity.org/),
[WODI](http://wodi.forge.ocamlcore.org/), [Fink](http://www.finkproject.org/),
[MacPorts](http://www.macports.org/)), or by building manually from source.

## Usage

- Compile the command-line interface by running `make` from the root
  directory.

- Print the command-line documentation by running `./bin/play --help`.

- Play a game by running `./bin/play`.

## Word lists

The
[TWL06](http://en.wikipedia.org/wiki/Official_Tournament_and_Club_Word_List)
(default) and [SOWPODS](http://en.wikipedia.org/wiki/SOWPODS) word lists are
included in `./share/data/`.
