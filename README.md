# PP

A DSL for probabilistic programming

## Building

    cabal sandbox init
    cabal configure
    cabal build

## Running the example

    cabal run

The example is a hidden markov model with two states, `A` and `B`.  Each state
generates observations from a standard normal distribution centered on 0 (for
`A`) and 5 (for `B`).

The probability of switching state is 0.75 - independent of the current state.

The example generates a list of observations, and infers both the "switching
probability" and the sequence of states that generated the observations.
