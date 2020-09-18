# Load an interactive environment:
{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs { }
, ghc ? "default"
}:
(import ./. { inherit sources pkgs ghc; }).interactive
