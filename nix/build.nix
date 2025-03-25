{
  callCabal2nix,
  overrideCabal,
}:
overrideCabal
(callCabal2nix "prompt-hs" ../. {})
(_: {configureFlags = ["-fprod"];})
