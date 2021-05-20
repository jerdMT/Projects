/*
 rotor.cpp refer to rotorORIGINAL.cpp
*/


#include <cstdlib>
using std::exit;

#include <iterator>
#include <algorithm>
using std::min;
using std::max;

#include <map>
using std::map;

#include <iostream>
using std::endl;
using std::cout;
using std::cerr;

#include <fstream>
using std::fstream;

#include <iomanip>
using std::setw;

#include <cmath>
using std::cosh;
using std::atan2;

#include <utility>
using std::make_pair;
using std::pair;

#include "mtrand.hpp"
mtrand R(1978); // seed for Mersenne generator

#include <time.h> 

// USR defined #of sites, CHANGES HERE ALSO IN rotor.h
const size_t COL=9;
const size_t ROWS=COL+1;
const size_t SITES=ROWS*COL;
double M=0;

double theta[SITES]; 
double torque[SITES];
double active[SITES];
double p[SITES];
double vals[3]={0.5,1,-0.5};
double vals1[3]={-0.5,0.5,1};


map<pair<int,int>,double> J;
//usr defined magnitude of disorder 
double D = 0.00001 ;
const double relaxation = 0.03;

//For original 90 sites

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

double Magnetization(void)
{

	for (int i = 0; i < SITES; ++i)
	{
		for (int j = 0; j < SITES; ++j)
		{
         	 M += p[i]*p[j]*(cos(theta[i])*cos(theta[j])+sin(theta[i])*sin(theta[j]));
		
		}
	}
	
	cout<< "Magnetization END: " << endl;

return M;
}

double x_pos(int i)
{
	return -0.9 + 0.212*(i%COL) + 0.106*((i/COL)%2 == 1);
}

double y_pos(int i)
{

	return 0.9 - 0.183597385602301*(i/COL);
}

int neighbour(int i, int n)
{	const int row=i/COL;
	
	if (n==0)
	{
	 if ((i%COL)==(COL-1)||i==(COL-1))
	  return i-(COL-1);
	 else 
	  return i+1;
	}

	else if (n==1)
	{
	 if ((i%COL)==0 || i==0)
	  return i+(COL-1);
	 else 
	  return i-1;
	}

	else if (n==2)
	{
	 if ((i%COL)==(COL-1) && row%2==1)
	  return i-((2*COL)-1);
	
	 else 
	  return (row%2 == 0 ? ((i+(COL*COL))%SITES) :i-(COL-1));
	}
	
	else if (n==3)
	{
	 if (i%COL==0 && row%2==0 && row>0)
	  return i-1;
	 else if (i==0)
	  return SITES-1;
	 else 
	  return (row%2==0 ? (i+(COL*COL)-1)%SITES : i-COL);
	}
	
	else if (n==4)
	{
	 if (i%COL==(COL-1) && i<(SITES-1) && row%2==1)
	  return i+1;
	 else if (i==(SITES-1))
	  return 0;
	 else 
	  return (row%2 == 0 ? i+COL : (i+(COL+1))%SITES);
	}

	else if (n==5)
	{
	 if (i%COL==0 && row%2==0)
	  return i+((2*COL)-1);
	 
	 else 
	  return (row%2 == 0 ? i+(COL-1) : (i+COL)%SITES);
	}
	

return 0;
}

void update(void)
{
   for (int i = 0; i < SITES; ++i)
   {
      if (active[i])
      { 
         torque[i] = 0.0;
         for (int n = 0; n < 6; ++n)
         { 
            const int j = neighbour(i,n);	
            if (active[j])
               torque[i] -= J[make_pair(min(i,j),max(i,j))]*sin(theta[i] - theta[j]);			
         }
      }
   }

   for (int i = 0; i < SITES; ++i)
     { if (active[i])
         theta[i] -= relaxation*torque[i];
     }	
}

double energy(void)
{
	double sum = 0.0;
	for (int i = 0; i < SITES; ++i)
	{
		if (active[i])
			for (int n = 0; n < 6; ++n)
			{
				const int j = neighbour(i,n);
				if (i < j and active[j])
					sum += J[make_pair(i,j)]*cos(theta[i] - theta[j]);
			}
	}
   
   return sum;    
}

void initialize(int argc, char* argv[]) 
{
   if (argc != 2)
   {
      cerr << "usage: rotor [disorder value D >= 0.0]" << endl;
      exit(1);
   }
   D = atof(argv[1]);
   assert(D >= 0.0);
   int k =0; // iterating through vals(1) over and over
	for (int i = 0; i < SITES; ++i)
	{
		theta[i] = 6.28318530717958647652*R();
		active[i] = true;
		for (int n = 0; n < 6; ++n)
		{ 
		   const int j = neighbour(i,n);
         if (i < j)	
         {
			   J[make_pair(i,j)] = pow(R(),D);	
            cout << "Adding exchange coupling #" << J.size() << endl;
         }
      		}

	
	const int row=i/COL;

  	 if(row%2==0) // generalized p_array
  	 {
  	 p[i]=vals[k];
  	 k=k+1;
  	 k=k%3;
  	 }
	
   	if(row%2==1)
   	{
   	 p[i]=vals1[k];
   	 k=k+1;
   	 k=k%3;
   	}

	  
cout<< "Site "<< i << " "<< "has value "<< p[i]<< endl ;

	
  	 

	}
   cout << "---" << endl;
  	
}
void rotor_random(void)
{
	for (int i = 0; i < SITES; ++i)
		theta[i] = 6.28318530717958647652*R();
}

void rotor_align(void)
{
	for (int i = 0; i < SITES; ++i)
		theta[i] = 0.0;

	
}

void rotor_vortex(void)
{
	for (int i = 0; i < SITES; ++i)
		theta[i] = atan2(x_pos(i),y_pos(i));
}

void toggle_lattice(void)
{
	if (!active[0])
		for (int i = 0; i < SITES; ++i)
			active[i] = true;
	else
	{
		for (int i = 0; i < SITES; i += 18)
			active[i] = active[i+3] = active[i+6] = 0;
		for (int i = 10; i < SITES; i += 18)
			active[i] = active[i+3] = active[i+6] = 0;
	}
}

void toggle_sign(void)
{	
   int n = 0;
   for (auto &x : J)
   {
      assert(x.first.first < x.first.second);
      cout << ++n << ": " << x.second << " -> ";
      x.second = -x.second;
      cout << x.second << endl;
   }
   cout << "---" << endl;
}

void exit_gracefully(void)
{
	cout << Magnetization()<< endl;
	
	fstream log;
	log.open ("data.txt",fstream::app); //append-mode, timestamp?
	if (log.is_open())
	{
	  log << M << "\t" << D << endl;
	  cout << "Data file updated."<< endl;
	  log.close();
	}
	else
	{ 
	  cout << "Failed to open file."<< endl;
	}

	exit(0);
}



