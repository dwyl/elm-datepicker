#!/usr/bin/env bash

elm make examples/Simple.elm --output=examples/example.js

elm-reactor &

$(npm bin)/cypress run

pkill elm-reactor
