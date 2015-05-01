#!/bin/bash

mkdir -p bin
rm bin/Demo
ghc -threaded -O0 -o bin/Demo *.hs 
rm -f *.hi
rm -f *.o
bin/Demo
