{
  description = "Additional Utilities for Working with Containers";

  inputs = {
    nixpkgs.url      = "github:nixos/nixpkgs/be44bf67"; # nixos-22.05 2022-10-15
    build-utils.url  = "github:sixears/flake-build-utils/r1.0.0.3";

    base1.url                = "github:sixears/base1/r0.0.9.4";
    more-unicode.url         = "github:sixears/more-unicode/r0.0.17.2";
    non-empty-containers.url = "github:sixears/non-empty-containers/r1.4.3.5";
    tasty-plus.url           = "github:sixears/tasty-plus/r1.5.2.4";
    textual-plus.url         = "github:sixears/textual-plus/r1.0.2.4";
  };

  outputs = { self, nixpkgs, flake-utils, build-utils
            , base1, more-unicode, non-empty-containers, tasty-plus
            , textual-plus }:
    build-utils.lib.hOutputs self nixpkgs "containers-plus" {
      deps = {
        inherit base1 more-unicode non-empty-containers tasty-plus textual-plus;
      };
      ghc = p: p.ghc8107; # for tfmt
    };
}
