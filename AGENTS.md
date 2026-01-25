# AGENTS.md

This file explains how multiple agents should collaborate when working on this project template.

## Collaboration Principles

- Read `README.md` and `CLAUDE.md` before making changes so everyone starts from the same context.
- Share intent upfront: outline the planned change, impacted modules, and verification strategy.
- Prefer incremental pull requests; leave large refactors for explicit coordination.
- Document open questions or risky assumptions directly in issues, commits, or PR descriptions.
- Leave the workspace clean: revert temporary tooling, keep scripts executable, and avoid committing editor artefacts.

## Quality Expectations

- Always run `./scripts/format.sh` and `./scripts/verify.sh` after touching Haskell sources.
- Use `./scripts/test.sh` for quick feedback; add or update tests whenever behaviour changes.
- Fix warnings instead of suppressing them. Update shared configuration rather than local overrides.
- Capture manual test steps or reproduction commands in the discussion when automated coverage is not available.

## Development Environment

- Enter the dev shell with `nix develop` (or use `direnv` for automatic loading).
- Use `./ghcid.sh` for fast iteration during feature work.
- Keep `flake.nix`, `cabal.project`, and `template.cabal` consistent when changing dependencies.

## Handover Checklist

- Summarise what changed, why, and any follow-up work that remains.
- Point to relevant files or modules by path so the next agent can resume quickly.
- Mention verification status (tests run, lint, manual checks) so others know the current state.
- Flag breaking changes or migrations early to give downstream consumers time to react.
