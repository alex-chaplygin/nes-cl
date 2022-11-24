CFLAGS=-fPIC -I/usr/include/SDL2

all: video.so

video.so: video.o
	gcc -shared video.o -lSDL2 -o video.so
