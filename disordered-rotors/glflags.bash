#!/bin/bash

if [ "$(uname)" == "Darwin" ]
then 
   echo -framework OpenGL -framework GLUT -lobjc -Wno-deprecated-declarations
else
   if [[ ! -z $(pkg-config --cflags --libs glut 2>&-) ]]
   then
      echo $(pkg-config --cflags --libs glut)
   else
      echo -lglut -lGLU -lGL -lm
   fi
fi

exit
