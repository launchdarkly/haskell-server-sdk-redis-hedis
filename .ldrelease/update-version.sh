#!/bin/bash

set -e

sed -i "s/^\(version:[^0-9]\+\).*/\1${LD_RELEASE_VERSION}/" package.yaml
sed -i "s/^\(version:[^0-9]\+\).*/\1${LD_RELEASE_VERSION}/" launchdarkly-server-sdk-redis-hedis.cabal
