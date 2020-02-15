#!/usr/bin/env bash

cd examples
elm make Simple.elm --output=example.js
cd ..

npx elm reactor &

$(npm bin)/cypress run

pkill elm
