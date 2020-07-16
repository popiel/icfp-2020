#!/usr/bin/env bash

set -euo pipefail

mkdir -p build
cd app
scalac *.scala -d ../build/Main.jar
