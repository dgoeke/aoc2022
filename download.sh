#! /usr/bin/env bash
#
# see: https://github.com/GreenLightning/advent-of-code-downloader

aocdl -output "resources/day{{.Day}}.txt" $@
