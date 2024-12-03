#!/bin/bash

part1() {
  grep -Eo 'mul\(\d{1,3},\d{1,3}\)' | tr -d 'mul()' | tr , '*' | paste -s -d+ - | bc
}

skip() {
  while read expr
  do
    case "$expr" in
      "do") unset skip ;;
      "don't") skip= ;;
      *) [ -z ${skip+x} ] && echo "$expr" ;;
    esac
  done
}

part2() {
  grep -Eo "mul\(\d{1,3},\d{1,3}\)|do(n't)?\(\)" | tr -d 'mul()' | tr , '*' | skip | paste -s -d+ - | bc
}

tee >(part1 1>&2) | part2 2>&1
