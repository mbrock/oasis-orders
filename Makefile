PORT ?= 8001

all: default.nix nix; nix-shell --run 'cabal build'
default.nix: oasis.cabal; cabal2nix . > default.nix
nix: default.nix shell.nix; nix-shell --run 'cabal configure'
repl: default.nix; nix-shell --run 'cabal repl'
hoogle:; nix-shell --run 'hoogle server --local -p $(PORT)'
