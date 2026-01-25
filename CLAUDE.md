# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Haskell project built with Nix flakes for reproducible development environments and haskell.nix for building. The project follows strict code quality standards with comprehensive warning flags and formatting requirements.

## Development Workflow

**ALWAYS run these commands after making changes to Haskell source:**
```bash
./scripts/format.sh
./scripts/verify.sh
```

These scripts ensure code formatting and successful builds. Never skip this step before committing changes.

## Development Commands

**Enter development shell:**
```bash
nix develop
```

**Build and run:**
```bash
nix run .
```

**Build with cabal (in nix shell):**
```bash
cabal build
cabal run blitz
```

**Live reload development:**
```bash
./ghcid.sh
```
Uses ghcid to watch for changes and rebuild/test automatically.

**Build Docker container:**
```bash
nix build .#container
```

## Code Quality Standards

**Formatting:**
- Always use ormolu for formatting (handled by `scripts/format.sh`)
- Never manually format - let the script handle it

**Warnings:**
- ALWAYS fix all compiler warnings - never ignore them
- Disable warnings in the cabal file's `common warnings` section, NEVER in source files
- The project uses `-Weverything` with specific exclusions for practical development

**Language Extensions:**
- Check the cabal file's `common extensions` section to see which extensions are enabled by default
- Current extensions include: OverloadedStrings, DuplicateRecordFields, NoFieldSelectors, StrictData, OverloadedRecordDot, AllowAmbiguousTypes, DataKinds, TypeFamilies, RecordWildCards, DeriveDataTypeable

## Architecture

**Project Structure:**
- `src/Main.hs` - Executable entry point that delegates to library
- `lib/` - Library modules with core functionality
- `scripts/` - Development automation scripts
- `flake.nix` - Nix flake defining the development environment and build

**Development Environment:**
- GHC 9.10.1 via haskell.nix
- Includes hlint, HLS, ormolu, and ghcid in development shell
- Comprehensive warning flags for strict code quality
- Uses GHC2021 language standard

**Dependencies:**
- Core dependencies: base, text, bytestring, async, unordered-containers
- Build tools: cabal, hlint, haskell-language-server available in nix shell

## Important Reminders

1. **Keep behaviour covered by automated tests; update coverage when behaviour changes**
2. **Always run format and verify scripts after changes**
3. **Fix all warnings - never disable them in source code**
4. **Use the cabal file to understand enabled language extensions**
5. **Enter nix develop shell for all development work**
6. **Use ghcid for continuous feedback during development**
7. **Run tests frequently - they should always pass**
8. **Test builds before committing any changes**
9. **NEVER make git commits - leave that to the user**
