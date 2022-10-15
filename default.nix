{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, lib, xmonad, xmonad-contrib
      }:
      mkDerivation {
        pname = "xmonad-lib";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ base containers xmonad xmonad-contrib ];
        license = lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
