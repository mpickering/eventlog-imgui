let np = import <nixpkgs> {}; in
np.mkShell { buildInputs = [ np.SDL2  np.pkgconfig np.haskell.compiler.ghc962  np.glew ]; }
