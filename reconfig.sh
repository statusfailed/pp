cabal2nix --shell `pwd` > shell.nix
#nix-shell -I ~ --command 'cabal configure --enable-library-profiling --enable-executable-profiling --enable-tests --enable-benchmarks'
nix-shell -I ~ --command 'cabal configure'
