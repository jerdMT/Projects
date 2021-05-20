#include <cstdlib>
using std::exit;

#include <iostream>
using std::endl;

#include <iomanip>
using std::setw;

#include <cmath>
using std::cosh;
using std::atan2;

#include "mtrand.hpp"
mtrand R(1975);

// 90 spins
double theta[90];
double torque[90];
double active[90];

double J = -1.0;

// we should make this work for arbitrary L and with a
// configuration that respects all the symmetries

//   00  01  02  03  04  05  06  07  08        row 0
//     09  10  11  12  13  14  15  16  17      row 1
//   18  19  20  21  22  23  24  25  26        row 2
//     27  28  29  30  31  32  33  34  35      row 3
//   36  37  38  39  40  41  42  43  44        row 4
//     45  46  47  48  49  50  51  52  53      row 5
//   54  55  56  57  58  59  60  61  62        row 6
//     63  64  65  66  67  68  69  70  71      row 7
//   72  73  74  75  76  77  78  79  80        row 8
//     81  82  83  84  85  86  87  88  89      row 9

double x_pos(int i)
{
	return -0.9 + 0.212*(i%9) + 0.106*((i/9)%2 == 1);
}

double y_pos(int i)
{
	return 0.9 - 0.183597385602301*(i/9);
}

int neighbour(int i, int n)
{
	const int row = i/9;
	if (n == 0) // right
		switch(i)
		{
			case 8:
			case 17:
			case 26:
			case 35:
			case 44:
			case 53:
			case 62:
			case 71:
			case 80:
			case 89: return i-8;
			default: return i+1;
		}
	else if (n == 1) // left
		switch(i)
		{
			case 0:
			case 9:
			case 18:
			case 27:
			case 36:
			case 45:
			case 54:
			case 63:
			case 72:
			case 81: return i+8;
			default: return i-1;
		}
	else if (n == 2) // up-right
		switch(i)
		{
			case 17:
			case 35: 
			case 53: 
			case 71: 
			case 89: return i-17;
			default: return (row%2 == 0 ? (i+81)%90 : i-8);
		}
	else if (n == 3) // up-left
		switch(i)
		{
			case 18: 
			case 36: 
			case 54: 
			case 72: return i-1;
			case 0: return 89;
			default: return (row%2 == 0 ? (i+80)%90 : i-9);
		}
	else if (n == 4) // down-right
		switch(i)
		{
			case 17:
			case 35: 
			case 53: 
			case 71: return i+1;
			case 89: return 0;
			default: return (row%2 == 0 ? i+9 : (i+10)%90);
		}
	else if (n == 5) // down-left
		switch(i)
		{
			case 0:
			case 18: 
			case 36: 
			case 54: 
			case 72: return i+17;
			default: return (row%2 == 0 ? i+8 : (i+9)%90);
		}
	return 0;
}

void update(void)
{
	for (int i = 0; i < 90; ++i)
	{
		if (active[i])
		{
			torque[i] = 0.0;
			for (int n = 0; n < 6; ++n)
			{
				const int j = neighbour(i,n);
				if (active[j])
					torque[i] += sin(theta[i] - theta[j]); // implement a small inertial term?
			}
			torque[i] *= -J; // implement J[i,j] as a map with values pow(R(),D)
		}
	}

	for (int i = 0; i < 90; ++i)
		if (active[i])
			theta[i] -= 0.003*torque[i];
}

double energy(void)
{
	double sum = 0.0;
	for (int i = 0; i < 90; ++i)
	{
		if (active[i])
			for (int n = 0; n < 6; ++n)
			{
				const int j = neighbour(i,n);
				if (i < j and active[j])
					sum += cos(theta[i] - theta[j]);
		}
	}
	return J*sum;
}

void initialize(void)
{
	for (int i = 0; i < 90; ++i)
	{
		theta[i] = 6.28318530717958647652*R();
		active[i] = true;
	}
}

void rotor_random(void)
{
	for (int i = 0; i < 90; ++i)
		theta[i] = 6.28318530717958647652*R();
}

void rotor_align(void)
{
	for (int i = 0; i < 90; ++i)
		theta[i] = 0.0;
}

void rotor_vortex(void)
{
	for (int i = 0; i < 90; ++i)
		theta[i] = atan2(x_pos(i),y_pos(i));
}

void toggle_lattice(void)
{
	if (!active[0])
		for (int i = 0; i < 90; ++i)
			active[i] = true;
	else
	{
		for (int i = 0; i < 90; i += 18)
			active[i] = active[i+3] = active[i+6] = 0;
		for (int i = 10; i < 90; i += 18)
			active[i] = active[i+3] = active[i+6] = 0;
	}
}

void toggle_sign(void)
{
		J = -J;
}

void exit_gracefully(void)
{
	exit(0);
}

