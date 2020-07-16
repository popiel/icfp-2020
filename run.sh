#!/usr/bin/env bash

set -euo pipefail

scala /solution/build/Main.jar "$@" || echo "run error code: $?"
