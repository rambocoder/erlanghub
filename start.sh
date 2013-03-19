#!/bin/sh
rebar compile
erl -pa $PWD/ebin -pa $PWD/deps/*/ebin +K true +A 4 -s erlanghub -config ../erlanghub.config -eval "io:format(\"http://localhost:19860/~n\")."
