//rotor spin array test 

using namespace std;
#include<iostream>

#include <utility>
using std::make_pair;
using std::pair;

#include<cmath>


int main()
{ //defining p values depending on A,B,C distinction 
 
 const size_t COL=9;
 const size_t ROWS=COL+1;
 const size_t SITES=ROWS*COL;
 double  p[SITES];
 double vals[3]={-0.5,1,0.5};
 double vals1[3]={1,-0.5,0.5};
 
 
int j=0;

for(int i=0;i<SITES;++i)
 {   int row=i/COL;

   if(row%2==0)
   {
   p[i]=vals[j];
   j=j+1;
   j=j%3;
   }
	
   if(row%2==1)
   {
    p[i]=vals1[j];
    j=j+1;
    j=j%3;
   }
	  	
	cout<<"Site "<< i << " has value "<< p[i] << endl;
  }
}      
