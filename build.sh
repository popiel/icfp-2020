#!/usr/bin/env bash

set -euo pipefail

mkdir build
cd app
scalac *.scala -d build/Main.jar
