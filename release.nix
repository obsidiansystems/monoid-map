{}:
let
  rp = import ./reflex-platform {};
  ghc = rp.ghc.override {
    overrides = self: super: {
      patch = self.callHackageDirect {
        pkg = "patch";
        ver = "0.0.8.0";
        sha256 = "1nnp7jn0vbx9zrnf57dxbknp6fbkqz7bca4i40aa6fabpwjw97kg";
      } {};
    };
  };
in ghc.callCabal2nix "monoid-map" ./. {}
