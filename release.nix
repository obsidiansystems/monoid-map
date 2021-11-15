{}:
let
  rp = import ./reflex-platform {};
in rp.ghc.callCabal2nix "bytestring-aeson-orphans" ./. {}
