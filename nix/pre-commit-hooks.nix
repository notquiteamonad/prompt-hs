{
  pre-commit-hooks-lib,
  system,
  nixpkgs,
}: let
  # Function to make a set of pre-commit hooks
  makeHooks = hooks:
    pre-commit-hooks-lib.lib.${system}.run {
      inherit hooks;
      src = ../.;
    };
  # Hooks which don't depend on running in a dev environment
  pureHooks = {
    alejandra.enable = true;
    hlint.enable = true;
    hpack = {
      enable = true;
      files = nixpkgs.lib.mkOverride 0 "(\.l?hs$)|cabal\.project|([^/]+\.cabal$)|(package\.yaml$)";
    };
    ormolu.enable = true;
  };
  # Hooks which can run on pre-commit but not in CI
  impureHooks = {};
in {
  pureHooks = makeHooks pureHooks;
  allHooks = makeHooks (pureHooks // impureHooks);
  tools =
    (with pre-commit-hooks-lib.packages.${system}; [
      alejandra
      hlint
      hpack
      ormolu
    ])
    ++ (with nixpkgs; []);
}
