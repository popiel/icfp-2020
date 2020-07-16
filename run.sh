#!/usr/bin/env bash

set -euo pipefail

scala /solution/app/build/Main.jar "$@" || echo "run error code: $?"
