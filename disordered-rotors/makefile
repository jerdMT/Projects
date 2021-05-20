#CC=g++ -O2 -ansi -pedantic -Wall -Wdeprecated-declarations
CC=g++ -O2 -std=c++17 -Wall -Wno-deprecated-declarations
GLFLAGS=`./glflags.bash`
#LIBS=`pkg-config --cflags --libs libpng`

## for Mac OS X
#GLFLAGS = -framework OpenGL -framework GLUT -lobjc

## For Linux (ugl-1xx series machines)

## For Linux (ugl-2xx series machines)
#INCL = -I/usr/local/MesaGL/include
#LDFLAGS = -L/usr/local/MesaGL/lib -lpthread
#GLFLAGS = -lglut -lGLU -lGL -lXmu -lXi -lXext -lX11 -lm 
EXE=rotor

all: $(EXE)

rotor: rotor.o rotor_openGL.o
	$(CC) -o $@ rotor.o rotor_openGL.o $(CFLAGS) $(GLFLAGS) $(LIBS)

rotor.o: rotor.cpp mtrand.hpp
	$(CC) -c rotor.cpp -o rotor.o

rotor_openGL.o: rotor_openGL.cpp rotor.h mtrand.hpp
	$(CC) -c rotor_openGL.cpp

clean:
	rm $(EXE) *.o
