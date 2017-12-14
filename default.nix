{ mkDerivation, aeson, base, base16-bytestring, bytestring
, data-dword, directory, hevm, lens, lens-aeson, pipes
, pipes-bytestring, pipes-group, stdenv, text, vector, wreq
}:
mkDerivation {
  pname = "oasis";
  version = "0.5";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base base16-bytestring bytestring data-dword directory hevm
    lens lens-aeson pipes pipes-bytestring pipes-group text vector wreq
  ];
  homepage = "https://github.com/dapphub/libethjet";
  description = "Binding to libethjet for Ethereum precompiled contracts";
  license = stdenv.lib.licenses.gpl3;
}
