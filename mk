#!/bin/bash

cabal build && time ./dist/build/euler/euler +RTS -xc -RTS $@
