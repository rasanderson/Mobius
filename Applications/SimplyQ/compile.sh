#!/bin/bash
g++ -fpic -m64 -std=c++11 -c -O2 simplyQ_dll.cpp
g++ -o simplyQ.so -m64 -shared simplyQ_dll.o
