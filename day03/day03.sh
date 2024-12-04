#!/bin/bash

part1() {
  grep -Eo 'mul\(\d{1,3},\d{1,3}\)' | tr -d 'mul()' | tr , '*' | \
  paste -s -d+ - | bc
}

part2() {
  grep -Eo "mul\(\d{1,3},\d{1,3}\)|do(n't)?\(\)" | tr -d 'mul()' | tr , '*' | \
  awk '$0 == "do" { skip = 0 } $0 == "don'\''t" { skip = 1 } !skip { print }' | \
  paste -s -d+ - | bc
}

tee >(part1 1>&2) | part2 2>&1
