{pkgs ? import <nixpkgs> {}}:
pkgs.haskellPackages.shellFor {
  packages = hpkgs: [
    (hpkgs.callPackage ./haskell-exps.nix {})
  ];
  nativeBuildInputs = with pkgs; [
    haskell-language-server
    cabal-install
  ];
  withHoogle = true;
}
