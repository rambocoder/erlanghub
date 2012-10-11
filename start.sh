#!/bin/sh
rebar compile
erl -pa $PWD/ebin -pa $PWD/deps/*/ebin +K true +A 4 -eval "io:format(\"http://localhost:19860/~n\")."