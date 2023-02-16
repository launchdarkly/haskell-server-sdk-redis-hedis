# Contributing to the LaunchDarkly Server SDK for Haskell - Redis Integration

LaunchDarkly has published an [SDK contributor's guide](https://docs.launchdarkly.com/sdk/concepts/contributors-guide) that provides a detailed explanation of how our SDKs work. See below for additional information on how to contribute to this SDK.

## Submitting bug reports and feature requests

The LaunchDarkly SDK team monitors the [issue tracker](https://github.com/launchdarkly/haskell-server-sdk-redis-hedis/issues) in the SDK repository. Bug reports and feature requests specific to this SDK should be filed in this issue tracker. The SDK team will respond to all newly filed issues within two business days.

## Submitting pull requests

We encourage pull requests and other contributions from the community. Before submitting pull requests, ensure that all temporary or unintended code is removed. Don't worry about adding reviewers to the pull request; the LaunchDarkly SDK team will add themselves. The SDK team will acknowledge all pull requests within two business days.

## Build instructions

### Prerequisites

The SDK depends on [PCRE](https://www.pcre.org/) and [`pkgconf`](https://github.com/pkgconf/pkgconf). You'll need to make sure these are installed.

If you use [Homebrew](https://brew.sh/) you can install the necessary packages by running the following command:

```
brew install pcre pkg-config
```

Additionally, you can install the `haskell-stack` package to install Stack via Homebrew.

### Building

```
stack build
```

### Testing

The tests will attempt to connect to a running redis instance on localhost:6379. You can run this via docker with

```
docker run --network host redis
```

Then you can execute the tests with

```
stack test
```
