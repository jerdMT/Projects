#include <iostream>
#include <fstream>
#include <math.h>
#include <vector> 
#include <cstdlib>
#include <string>
#include <algorithm>
#include <map>
#include <utility>
#include <chrono>
#include <numeric>
#include "LinkingNumber.h" //header file with all fucntions
#define n 3                
#define pi 3.141593
#define chains 1600
#define bxx 90.9746
#define bxy 90.9746 //periodic box lengths
#define bxz 90.9746


int main()
{ 	
 
	int NumberofChainPairs;
	std::cout << "Enter 1 for LKP of single pair, or Enter 1<n<1600 for LKP of all possible chain combinations up to n: " ;
	std::cin >> NumberofChainPairs;
	
    using std::chrono::high_resolution_clock;
    using std::chrono::duration_cast;
    using std::chrono::minutes;
  //testing for two 2 chains, of 400 monomers each
  int n_vertices1 = 100;  //For scanning function, segment length of 1st chain
  int n_vertices2 = 100; // segment length of second chain 
  
  std::vector<std::vector<double>> C_all;
  std::vector<std::vector<double>> D_all;

  std::vector<double> line;              // storing coordinates(both chains)
  std::vector<std::vector<double>> DATA; 
  std::vector<double> lkp_VALUES;
  std::vector<double> m{0, 0, 0};
  std::map<std::pair<int, int>, double> lk_VALUES;
  std::vector<double> scanVALUES;
  std::pair<int, int> indx_max;
  std::vector<std::pair<int,int>> indices;
  
  double num;
  const int columns = 8;

  std::ifstream DataStream;
  DataStream.open("DATASORTED.txt");
  if (DataStream.is_open())
  {

    while (DataStream >> num) //loop running until end of file
    {
      line.push_back(num);

      if (line.size() == columns)
      {
        DATA.push_back(line);
        line.clear();
      }
    }
  }
  else
  {
    std::cout << "ERROR OPENING FILE." << std::endl;
  }

  //data size 640,000: 1600 chains 400 monomers each.
  std::vector<double> x;
  std::vector<double> y;
  std::vector<double> z;

  //std::cout << "# of coord. entries: " << DATA.size() << std::endl;

  for (double i = 0; i < DATA.size(); ++i)
  {
    x.push_back(DATA[i][2]);
  }

  for (double i = 0; i < DATA.size(); ++i)
  {
    y.push_back(DATA[i][3]);
  }

  for (double i = 0; i < DATA.size(); ++i)
  {
    z.push_back(DATA[i][4]);
  }

  //number of sub-vector of size 400 =(1600)
  int size = 1600;

  //array of vectors for sub-vectors
  std::vector<double> chainX[size];
  std::vector<double> chainY[size];
  std::vector<double> chainZ[size];

  for (int k = 0; k < size; ++k)
  {
    // get range for next set of n elements
    auto start_itr = std::next(x.cbegin(), k * 400);
    auto end_itr = std::next(x.cbegin(), k * 400 + 400);

    // allocate memory for the sub-vector
    chainX[k].resize(400);

    // code to handle the last sub-vector as it might
    // contain less elements
    if (k * 400 + 400 > x.size())
    {
      end_itr = x.cend();
      chainX[k].resize(x.size() - k * 400);
    }

    // copy elements from the input range to the sub-vector
    std::copy(start_itr, end_itr, chainX[k].begin());
  }

  // print the sub-vectors

  // print(chainX[9]);

  for (int k = 0; k < size; ++k)
  {
    // get range for next set of n elements
    auto start_itr = std::next(y.cbegin(), k * 400);
    auto end_itr = std::next(y.cbegin(), k * 400 + 400);

    // allocate memory for the sub-vector
    chainY[k].resize(400);

    // code to handle the last sub-vector as it might
    // contain less elements
    if (k * 400 + 400 > y.size())
    {
      end_itr = y.cend();
      chainY[k].resize(y.size() - k * 400);
    }

    // copy elements from the input range to the sub-vector
    std::copy(start_itr, end_itr, chainY[k].begin());
  }

  // print(chainY[9]);

  for (int k = 0; k < size; ++k)
  {
    // get range for next set of n elements
    auto start_itr = std::next(z.cbegin(), k * 400);
    auto end_itr = std::next(z.cbegin(), k * 400 + 400);

    // allocate memory for the sub-vector
    chainZ[k].resize(400);

    // code to handle the last sub-vector as it might
    // contain less elements
    if (k * 400 + 400 > z.size())
    {
      end_itr = z.cend();
      chainZ[k].resize(z.size() - k * 400);
    }

    // copy elements from the input range to the sub-vector
    std::copy(start_itr, end_itr, chainZ[k].begin());
  }

  std::fstream rings;
       rings.open ( "RINGS.txt", std::fstream::app);  
	
	
	//storing largest linking number value and associated segment length
	if (rings.is_open())
	{
	for (int p = 0; p<400;p++){
	rings << chainX[9][p] << " " << chainY[9][p] << " " << chainZ[9][p]<< " " << chainX[45][p] <<" "<<  chainY[45][p] << " "<<chainZ[45][p]<< " " << ((chainY[45][p])+90.9746) << std::endl;
	}
	}
	else
	{
	  std::cout<< "Failed to open file. " << std::endl;   
	
	}

  

  
  //  std::cout<< "Linking Number: " << compute_lk(chainX[id],chainY[id],chainZ[id],chainX[id1],chainY[id1],chainZ[id1],m,n_vertices)<<std::endl;
   

   
   
   std::ofstream STREAM;
  	STREAM.open("LKPLOG.txt", std::fstream::app);
   std::ofstream stats;
  	stats.open("LinkingStats.txt", std::fstream::app);
  	
  	
  
   if (NumberofChainPairs==1)
{  
   int a,b;
   std::cout << std::endl << "Enter the 1st chain identifier for the pair" << std::endl;
   std::cin >> a;
   std::cout << "Enter the 2nd chain identifier for the pair" << std::endl;
   std::cin >> b;
	
   int id=a-1;
   int id1=b-1;

STREAM << "(" << a << "," << b << ")" <<std::endl;
  auto start = high_resolution_clock::now();
  compute_lkp(chainX[id], chainY[id], chainZ[id], chainX[id1], chainY[id1], chainZ[id1], lkp_VALUES, n_vertices1,n_vertices2);
  auto stop = high_resolution_clock::now();
  auto duration = duration_cast<minutes>(stop - start);
  
  std::cout << "Time taken by function: "
         << duration.count() << "  minutes" << std::endl;
       
}

 else if (NumberofChainPairs > 1)         
{  
	std::ifstream inFile;
 inFile.open("LinkingStats.txt");
 

 int linking_test[5050];
 int coord_i[5050];
 int coord_j[5050];
 if (!inFile) {
        std::cout << "Unable to open file";
        exit(1); // terminate with error
    }
 for (int i = 0; i<5049; i++) //how ever many lines done already in LinkingStats
 {
     
     inFile >> linking_test[i];
     inFile >> coord_i[i];
     inFile >> coord_j[i];
     
     std::pair<int,int> presentPair(coord_i[i]-1,coord_j[i]-1); // why the -1?
     indices.push_back(presentPair); 
 }

 auto start = high_resolution_clock::now();
  for(int i=0; i<NumberofChainPairs; i++) // i was changed here to cover the second 100-block
  {
     //i is ID
     
     
      	  
      for (int j=0; j<=NumberofChainPairs; j++)
      { 	//j is ID1
       

       std::pair<int,int> indx(i,j);
       std::pair<int,int> indx1(j,i);
       bool isPresent=std::find(indices.begin(), indices.end(), indx) !=indices.end();
      	
      	indices.push_back(indx);
      	indices.push_back(indx1);
      	
      	

      	
      	
      	if (i!=j && isPresent==0)
       {
        STREAM <<"(" << i+1 << "," << j+1 <<")"<< std::endl;
      	std::cout<< "Chain " << i+1 << "&" << j+1 << std::endl;	
      	double lkp=compute_lkp(chainX[i], chainY[i], chainZ[i],chainX[j],chainY[j],chainZ[j], lkp_VALUES,n_vertices1, n_vertices2); 
         //stored as linking logic value, id1, id2
         stats << " " << i+1 << " " << j+1 <<  std::endl;
       }
       
      } 
  } 
  auto stop = high_resolution_clock::now();
  auto duration = duration_cast<minutes>(stop - start);
  
  std::cout << "Time taken by function: "
         << duration.count() << "  minutes" << std::endl;
}

else
{
 std::cout<< "invalid entry for the number of chain pairs."<< std::endl;
}         
  //Next step: nested for loop that runs over ALL pairs of chains and calls lkp over the chain


  

  return 0;
}
