#include <cstddef>
using std::size_t;

#include <cassert>

#ifdef __APPLE__
#include <GLUT/glut.h>
#else
#include <GL/glut.h>
#endif

#include <iostream>
using std::ostream;
using std::endl;
using std::fixed;

#include <vector>
using std::vector;

#include <utility>
using std::pair;
using std::make_pair;

#include <sstream>
using std::stringstream;

#include <algorithm>
using std::min;

#include <cmath>
using std::cos;
using std::sin;
#include <cstring>

#include "rotor.h"

int width = 640;
int height = 640;

void glutPrint(float x, float y, const char* text, float r, float g, float b, float a) 
{ 
    if(!text || !strlen(text)) return; 
    bool blending = false; 
    if(glIsEnabled(GL_BLEND)) blending = true; 
    glEnable(GL_BLEND); 
    glColor4f(r,g,b,a); 
    glRasterPos2f(x,y); 
    while (*text) { 
        glutBitmapCharacter(GLUT_BITMAP_9_BY_15, *text); 
        text++; 
    } 
    if(!blending) glDisable(GL_BLEND); 
}

void display()
{
	glClear(GL_COLOR_BUFFER_BIT);
	for (int i = 0; i < 90; ++i)
	{
		if (active[i])
			for (int n = 0; n < 6; ++n)
			{
				const int j = neighbour(i,n);
				if (i < j and active[j] and fabs(x_pos(i) - x_pos(j)) < 0.4 and fabs(y_pos(i) - y_pos(j)) < 0.4)
				{
					glBegin(GL_LINES);
					glColor3f(0.5,0.6,1.0);
					glVertex2f(x_pos(i),y_pos(i));
					glVertex2f(x_pos(j),y_pos(j));
					glEnd();
				}
			}
	}
	
	for (int i = 0; i < 90; ++i)
	{
		if (active[i])
		{
			const double s = sin(theta[i]);
			const double c = cos(theta[i]);
			glBegin(GL_POLYGON);
			glColor3f(1.0,0.95,0.95);
			glVertex2f(x_pos(i)-0.015*c-0.04*s,y_pos(i)+0.015*s-0.04*c);
			glVertex2f(x_pos(i)+0.015*c-0.04*s,y_pos(i)-0.015*s-0.04*c);
			glVertex2f(x_pos(i)+0.1*s,y_pos(i)+0.1*c);
			glEnd();
		}
	}

	for (int i = 0; i < 90; ++i)
	{
		glPointSize(5.0); 
		glBegin(GL_POINTS);
		glColor3f(1,0.4,0.1);
		glVertex2f(x_pos(i),y_pos(i));
		glEnd();
	}
	glEnd();
	glFlush();


	stringstream s;
	s << "Energy: " << fixed << energy();
	glutPrint(-1.0F+50.0F/width, -1.0F+50.0F/height,s.str().c_str(), 1.0f, 1.0f, 1.0f, 0.5f);

	

	glFlush();
	glutSwapBuffers();
}

bool moving = false;
int startx;

void mouse(int button, int state, int x, int y)
{
	if (button == GLUT_LEFT_BUTTON) 
	{
		if (state == GLUT_DOWN) 
		{
			moving = true;
			startx = x;
		}
		if (state == GLUT_UP) 
			moving = false;
	}
}

void motion(int x, int y)
{
	if (moving) 
	{
		const double angle = 9.42*(x-startx)/width;
		for (int i = 0; i < 90; ++i)
			theta[i] += angle;
		startx = x;
		glutPostRedisplay();
	}
}

void keyboard(unsigned char key, int x, int y)
{
	if (key == 'r')
		rotor_random();
	else if (key == 'a')
		rotor_align();
	else if (key == 'v')
		rotor_vortex();
	else if (key == 'l')
		toggle_lattice();
	else if (key == 'j')
		toggle_sign();
	else if (key == 'q')
		exit_gracefully();	
}

void changeSize(int w, int h) 
{
	width = w;
	height = h;
    glViewport(0, 0, w, h);
}

const int delay = 1; // milliseconds
void animate(int)
{
	update();
	glutPostRedisplay();
	glutTimerFunc(delay,animate,0);
}

int main(int argc, char **argv)
{
	glutInit(&argc,argv);
	initialize(argc,argv);
	glutInitDisplayMode (GLUT_RGB | GLUT_DOUBLE);
	glutInitWindowSize (640, 640);
	glutInitWindowPosition (100, 100);
	glutCreateWindow("Lattice of interacting rotors");
	glutDisplayFunc(display);
	glutMouseFunc(mouse);
	glutMotionFunc(motion);	
	glutKeyboardFunc(keyboard);
	glutTimerFunc(delay,animate,0);
	glutReshapeFunc(changeSize);	
	glutMainLoop();
	std::cout << Magnetization()<< endl;
}


