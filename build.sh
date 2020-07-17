#!/usr/bin/env bash

set -euo pipefail

mkdir -p build
cd src/main/scala
scalac *.scala -d ../../../build/Main.jar
