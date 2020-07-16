#!/bin/sh

scala /solution/app/build/Main.jar "$@" || echo "run error code: $?"
