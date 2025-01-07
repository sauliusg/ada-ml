#!/bin/sh

bzcat tests/data/p12.ppm.bz2 | ./bin/pnmread /dev/stdin
