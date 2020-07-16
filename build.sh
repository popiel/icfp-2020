#!/bin/sh

cd app
mkdir build
scalac *.scala -d build/Main.jar
