{ nixpkgs ? import ./nix/nixpkgs.nix, nixpkgsArgs ? {}, compiler ? "ghc802" }:

with rec {
  pkgs = (import nixpkgs) nixpkgsArgs;

  inherit (pkgs) haskell;

  # Compute, e.g.: "x86_64-linux-ghc-8.0.2"
  computeHaskellDir = hp: pkg: "${pkg.system}-${hp.ghc.name}";

  setHaddockStyle = hp: pkg: (
    with rec {
      fetchJQuery = { version, sha256 }: pkgs.fetchurl {
        url = "https://code.jquery.com/jquery-${version}.min.js";
        inherit sha256;
      };

      jquery = fetchJQuery {
        version = "3.2.1";
        sha256  = "1pl2imaca804jkq29flhbkfga5qqk39rj6j1n179h5b0rj13h247";
      };
    };

    haskell.lib.overrideCabal pkg (old: rec {
      preInstall = (''
        for haddockDir in ./dist/doc/html/*; do
             if test -d "$haddockDir"; then
                 rm -fv "$haddockDir/ocean.css"
                 rm -fv "$haddockDir/haddock-util.js"
                 cp -v "./misc/haddock.css" "$haddockDir/ocean.css"
                 cp -v "./misc/haddock.js"  "$haddockDir/haddock-util.js"
                 ln -sv ${jquery} "$haddockDir/jquery.js"
             fi
        done
      '' + (old.preInstall or ""));
    }));

  addHydraHaddock = hp: pkg: (
    with rec {
      suffix = "share/doc/${computeHaskellDir hp pkg}/${pkg.name}/html";
    };

    haskell.lib.overrideCabal pkg (old: rec {
      doHaddock = true;
      postInstall = ((old.postInstall or "") + ''
        mkdir -pv "$out/nix-support"
        echo "doc haddock $out/${suffix} index.html" \
            >> "$out/nix-support/hydra-build-products"
      '');
    }));

  addHydraHPC = hp: pkg: (
    haskell.lib.overrideCabal pkg (old: rec {
      doCoverage = true;

      postInstall = ((old.postInstall or "") + ''
        mkdir -pv "$out/nix-support"
        echo "report hpc $out/share/hpc/dyn/html/${pkg.name} hpc_index.html" \
            >> "$out/nix-support/hydra-build-products"
      '');
    }));

  hp = haskell.packages.${compiler}.override {
    overrides = self: super: (
      with haskell.lib;
      with { cp = file: (self.callPackage (./nix/haskell + "/${file}") {}); };

      {
        hydra-badges = (cp "hydra-badges.nix").overrideDerivation (old:
          with {
            sf = name: type: let bn = baseNameOf (toString name); in !(
              (type == "directory" && (bn == ".git"))
              || pkgs.lib.hasSuffix "~" bn
              || pkgs.lib.hasSuffix ".o" bn
              || pkgs.lib.hasSuffix ".so" bn
              || pkgs.lib.hasSuffix ".nix" bn
              || (type == "symlink" && pkgs.lib.hasPrefix "result" bn));
          };
          { src = builtins.filterSource sf ./.; });
      }
    );
  };
};

setHaddockStyle hp
  (addHydraHaddock hp
  (addHydraHPC hp hp.hydra-badges))
