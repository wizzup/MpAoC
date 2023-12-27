{
  description = "Megaparsec for Advent of code";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  nixConfig.bash-prompt-suffix = "🚀";
  outputs = {
    self,
    nixpkgs,
  }: (let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};

    drv = pkgs.haskellPackages.developPackage {root = ./.;};

    hsPcks = with pkgs.haskellPackages; [
      cabal-install
      ghcid
      haskell-language-server
      implicit-hie
      hpack
      doctest
    ];

    dev = drv.env.overrideAttrs (attr: {
      buildInputs =
        attr.buildInputs
        ++ hsPcks
        ++ [pkgs.helix];

      shellHook = ''
        source ${pkgs.cabal-install}/share/bash-completion/completions/cabal 
      '';
    });
  in
    with pkgs; {
      formatter.${system} = alejandra;
      packages.${system} = {
        default = drv;
      };
      devShells.${system}.default = dev;
    });
}
