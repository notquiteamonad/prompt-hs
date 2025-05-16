{
  description = "Development infrastructure for prompt-hs";
  inputs = {
    nixpkgs-src.url = "github:NixOS/nixpkgs?rev=b62d2a95c72fb068aecd374a7262b37ed92df82b";
    pre-commit-hooks-lib = {
      inputs.nixpkgs.follows = "nixpkgs-src";
      url = "github:cachix/pre-commit-hooks.nix";
    };
  };
  outputs = {
    nixpkgs-src,
    pre-commit-hooks-lib,
    self,
    ...
  }: let
    systemsHelpers = import nix/systems.nix;
    allSystems = nixpkgs-src.lib.platforms.all;
    supportedSystems = with systemsHelpers.system allSystems; [x86_64-linux aarch64-linux];
  in
    systemsHelpers.forEachSystem supportedSystems (system: let
      nixpkgs = nixpkgs-src.legacyPackages.${system};
      preCommitHooks = import nix/pre-commit-hooks.nix {
        inherit pre-commit-hooks-lib nixpkgs system;
      };
      targets = builtins.map (target: "ghc" + target) ["810" "90" "92" "94" "96" "98"];
      currentTarget = nixpkgs.lib.lists.last targets;
      haskellPackagesFor = target: nixpkgs.haskell.packages.${target};
      haskellEnv = (haskellPackagesFor currentTarget).ghcWithPackages (pkgs: with pkgs; [cabal-install haskell-language-server]);
    in {
      checks.pre-commit-check = preCommitHooks.pureHooks;
      devShells.default = nixpkgs.mkShell {
        buildInputs = [haskellEnv] ++ preCommitHooks.tools;
        inherit (preCommitHooks.allHooks) shellHook;
      };
      lib = {
        inherit targets;
      };
      packages = let
        packages' = builtins.listToAttrs (
          builtins.map (target: {
            name = "prompt-hs-${target}";
            value = import nix/build.nix {
              inherit (haskellPackagesFor target) callCabal2nix;
              inherit (nixpkgs.haskell.lib) overrideCabal;
            };
          })
          targets
        );
      in
        packages'
        // {
          default = packages'.${"prompt-hs-" + currentTarget};
          # runChecks is a hack required to allow checks to run on a single system
          # when using Import from Deviation (https://discourse.nixos.org/t/nix-flake-check-for-current-system-only/18366)
          # Building it is the single-system equivalent of running "nix flake check".
          runChecks = nixpkgs.runCommand "run-checks" {
            currentSystemChecks = builtins.attrValues self.checks.${system};
          } "echo $currentSystemChecks; touch $out";
        };
    });
}
