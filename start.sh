#!/bin/sh
erl -pa ebin deps/*/ebin -name template@localhost -s template -eval "io:format(\"Point your browser at http://localhost:8080~n\")."
