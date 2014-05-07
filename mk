#!/bin/bash

FLAGS=

if [ $1 == "--debug" ]
then
	shift
	FLAGS='+RTS -xc -RTS'
fi

cabal build && time ./dist/build/euler/euler $FLAGS $@
