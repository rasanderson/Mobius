#!/bin/bash
g++ -fpic -m64 -std=c++11 -c -O2 simplyn_dll.cpp
g++ -o simplyp.so -m64 -shared simplyn_dll.o
