#!/bin/bash
g++ -fpic -m64 -std=c++11 -c -O2 simplycnp_dll.cpp
g++ -o simplyCNP.so -m64 -shared simplycnp_dll.o
