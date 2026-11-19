# Change log

All notable changes to the LaunchDarkly SDK for Haskell - Redis Integration will be documented in this file. This project adheres to [Semantic Versioning](https://semver.org).



## [1.0.1](https://github.com/launchdarkly/haskell-server-sdk-redis-hedis/compare/1.0.0...1.0.1) (2026-11-19)


### Bug Fixes

* Build against launchdarkly-server-sdk 4.6.2, which does not need a system PCRE library ([ca55727](https://github.com/launchdarkly/haskell-server-sdk-redis-hedis/commit/ca55727aa7f4b3a4035c87a0a87f758f2517454e))

## [1.0.0] - 2023-02-21
### Added:
- Moved the previous redis integration package out of the [Haskell SDK repository](https://github.com/launchdarkly/haskell-server-sdk) into this standalone package.

### Changed:
- Updated default namespace from "LaunchDarkly" to "launchdarkly" to match other redis integration defaults.
- Updated packages to minimums allowed by SDK.
