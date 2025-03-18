#pragma once

#define MLS_NAMESPACE mlspp
#define DISABLE_GREASE ON

// TODO: make this configurable in cabal flags
// At least on Windows Mingw, using ghcup run -m -- pacman -S openssl-devel
// installs OpenSSL 3, and if we don't put this define, it won't work
#define WITH_OPENSSL3 ON
