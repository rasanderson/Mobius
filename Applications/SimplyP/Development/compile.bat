@echo off

g++ -c -m64 -std=c++11 -O2 simplyp_dev_dll.cpp -fexceptions -fmax-errors=5
g++ -o simplyp_dev.dll -static -static-libgcc -static-libstdc++ -s -shared simplyp_dev_dll.o -Wl,--subsystem,windows -luuid -lole32 -loleaut32