#!/bin/bash
x86_64-w64-mingw32-gcc -c -m64 -std=c++11 -O2 simplyQ_dll.cpp -fexceptions -fmax-errors=5
#x86_64-w64-mingw32-gcc -o simplyQ.dll -static -static-libgcc -static-libstdc++ -s -shared simplyQ_dll.o -Wl,--subsystem,windows -luuid -lole32 -loleaut32
x86_64-w64-mingw32-gcc -o simplyQ.dll -static -static-libgcc -static-libstdc++ -s -shared simplyQ_dll.o -Wl,--subsystem,windows -luuid
