CFLAGS=-fPIC -I/usr/include/SDL2

all: /tmp/video.so

/tmp/video.so: video.o
	gcc -shared video.o -lSDL2 -o /tmp/video.so
