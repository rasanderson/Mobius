@echo off

g++ -c -m64 -std=c++11 -O2 simplyc_regional_old_dll.cpp -fexceptions -fmax-errors=5
g++ -o simplyc_regional_old.dll -static -static-libgcc -static-libstdc++ -s -shared simplyc_regional_old_dll.o -Wl,--subsystem,windows -luuid -lole32 -loleaut32