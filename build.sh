#!/usr/bin/env sh
set -xe

cobc -x main.cob -o brickbreak -lraylib -Q "-Wl,--no-as-needed"
