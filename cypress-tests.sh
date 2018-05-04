#!/usr/bin/env bash

elm make examples/Main.elm --output=examples/example.js

elm-reactor &

$(npm bin)/cypress run

pkill elm-reactor
