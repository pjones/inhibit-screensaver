{ sources ? import nix/sources.nix
, pkgs ? import sources.nixpkgs { }
, nix-hs ? import sources.nix-hs { inherit pkgs; }
, ghc ? "default"
, static ? false
}:

nix-hs {
  cabal = ./inhibit-screensaver.cabal;
  compiler = ghc;
  flags = [ "maintainer" ];
  enableFullyStaticExecutables = static;
}
