# liars-poker

Uses random sampling to compute the likelihood of larger hands in liars poker.

## Installation

After installing [Leiningen](http://leiningen.org/), do the following:

    git clone https://github.com/yuzeh/liars-poker.git
    cd liars-poker
    lein run

You can also `lein uberjar` to bundle and then execute the resulting bundle.

## Options

    Switches           Default  Description
    --------           -------  -----------
    -n, --num-cards    20       Number of cards
    -s, --num-samples  1000     Number of samples
    -p, --print-every  1000     How often to print results

## License

Copyright © 2013 Dan Huang.

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
