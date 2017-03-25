{ mkDerivation, aeson, base, bytestring, data-default, exceptions
, free, http-conduit, HUnit, protolude, singletons, stdenv
, test-framework, test-framework-hunit, text
}:
mkDerivation {
  pname = "haskell-etcd";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring data-default exceptions free http-conduit
    protolude singletons text
  ];
  testHaskellDepends = [
    aeson base bytestring HUnit protolude test-framework
    test-framework-hunit text
  ];
  homepage = "https://github.com/githubuser/haskell-etcd#readme";
  license = stdenv.lib.licenses.lgpl3;
}
