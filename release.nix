{}:
let
  rp = import ./reflex-platform {};
in rp.ghc.callCabal2nix "monoid-map" ./. {}
