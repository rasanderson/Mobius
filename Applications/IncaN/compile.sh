#!/bin/bash
g++ -fpic -m64 -std=c++11 -c -O2 incan_dll.cpp
g++ -o incan.so -m64 -shared incan_dll.o
