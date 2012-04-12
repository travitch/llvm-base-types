This package defines types used in the llvm-analysis and
llvm-data-interop packages.

This is a separate package mostly because llvm-data-interop needed to
be split out due to C++ linkage issues.  llvm-data-interop needs the
definitions in this packages but cannot depend on llvm-analysis (which
also needs these types).

Further, to avoid code duplication some of the C++ enumeration values
used in llvm-data-interop are actually included with this package.
The definitions in this package are used (via c2hs) to build Haskell
equivalents.  The base types need these definitions, but the
llvm-data-interop package also needs to be able to find the header.
This is handled in Setup.hs in llvm-data-interop with cooperation from
this package (which exports the cabal-generated path to the installed
header).

Only llvm-data-interop and llvm-analysis should ever need to reference
this package since llvm-analysis re-exports all of the definitions.
