cabal2nix --shell `pwd` > shell.nix
nix-shell -I ~ --command 'cabal configure'
