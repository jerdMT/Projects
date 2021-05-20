#define n 3
#define pi 3.141593
#define chains 1600
#define bxx 90.9746
#define bxy 90.9746
#define bxz 90.9746


// Utility function to print a vector
bool isNumeric(std::string str) {
   for (int i = 0; i < str.length(); i++)
      if (isdigit(str[i]) == false)
         return false; //when one non numeric value is found, return false
      return true;
}

template<typename T>
void print(std::vector<T> const &v)
{
    for (auto &i: v)
        std::cout << i << std::endl;
 
   
}
template<typename T>
std::vector<T> slice(std::vector<T> const &v, int beg, int end)
{
    auto first = v.cbegin() + beg;
    auto last = v.cbegin() + end + 1;
 
    std::vector<T> vec(first, last);
    return vec;
}
std::vector<double> vecAdd(std::vector<double> originalVec, double num)
{ 
  std::vector<double> transVec;
  for(double& x : originalVec) 
   	{ 
   	   transVec.push_back(x += num);  // add num to every element of the vector
   	   				  // and create new translated vector
   	}
   	
   	return transVec;
}

// Function that returns 
// dot product of two vector arrays
double dotProduct(std::vector<double> vect_A, std::vector<double> vect_B) 
{ 
  
    double product = 0; 
  
    // Loop to calculate dot product 
    for (int i = 0; i < n; i++) 
  
        product = product + vect_A[i] * vect_B[i]; 
    return product; 
} 
  
// Function to find 
// cross product of two vector arrays. 

void crossProduct(std::vector<double>& vect_A, std::vector<double>& vect_B, std::vector<double>& cross_P ) 
{ 
	
    cross_P[0] = vect_A[1] * vect_B[2] - vect_A[2] * vect_B[1]; 
    cross_P[1] = vect_A[2] * vect_B[0] - vect_A[0] * vect_B[2]; 
    cross_P[2] = vect_A[0] * vect_B[1] - vect_A[1] * vect_B[0]; 
} 

int sign(double x)
{
	if (x>0) return 1;
	if (x<0) return -1;
	return 0;
}

double frobeniusNorm(std::vector<double> X)
{
	double sumSq=0;
	for(int k=0; k<n; k+=1)
	{	
 	  sumSq += pow(X[k],2);			
	}
		
	double res=sqrt(sumSq);
	return res;

}

double map_MAX(std::map<std::pair<int,int>,double> &lk_VALUES, std::pair<int,int> &indx_max)
{
	
	double currentMax = 0;
	for (auto const &kv: lk_VALUES) 
	{
 	    //std::cout << "(" << kv.first.first << ", " << kv.first.second << ") = " << kv.second << std::endl;

  	     if(kv.second>currentMax)
  	     {
  	     	indx_max=std::make_pair (kv.first.first, kv.first.second);
  	     	currentMax= kv.second;
  	     
  	     }
  	     
	}
std::cout<<"Indicies of chain segments that produce maximum linking:  " << indx_max.first << "," << indx_max.second  << '\n';	
std::cout<<"Maximal map element (Largest linking#): "<< currentMax << std::endl;
return currentMax; 	
}

double  gauss_lk( std::vector<double> a1, std::vector<double> a2, std::vector<double> b1, std::vector<double> b2)
{	
		
		std::vector<double> cross_P(3) ;
	
		
		std::vector<double> ra (3);
		std::vector<double> rb (3);
		
		std::vector<double> r00 (3); // edges from vertices
		std::vector<double> r01 (3);
		std::vector<double> r10 (3);
		std::vector<double> r11 (3);
		
		std::vector<double> V1 (3); //containers for crossproduct
		std::vector<double> V2 (3);
		std::vector<double> V3 (3);
		std::vector<double> V4 (3);
		std::vector<double> aux1 (3);
		
	
		//vector subtraction
		for(int i=0;i<n;i+=1)
		{
			r00[i]=a1[i]-b1[i];
			r01[i]=a1[i]-b2[i];
			r10[i]=a2[i]-b1[i]; //problem is here.
			r11[i]=a2[i]-b2[i]; // consider using GSL
			ra[i]=a2[i]-a1[i];  // library has better
			rb[i]=b2[i]-b1[i];  //functions
			
			
		}
		
		
		crossProduct(r00, r01, cross_P);
		for (int j=0;j<n;j+=1)
		{   
		   V1[j]=cross_P[j];
		   //filled correctly
		   
		}
		
		crossProduct(r01, r11, cross_P);
		for (int j=0;j<n;j+=1)
		{   
		   V2[j]=cross_P[j];
		   //filled correctly
		}
		
		crossProduct(r11, r10, cross_P);
		for (int j=0;j<n;j+=1)
		{   

		   V3[j]=cross_P[j];
		   //filled correctly
		   
		}
		
		
		crossProduct(r10, r00, cross_P);
		for (int j=0;j<n;j+=1)
		{   
		
		   V4[j]=cross_P[j];
		   //filled correctly
		   
		}
		
		double normV1=frobeniusNorm(V1);
		double normV2=frobeniusNorm(V2);
		double normV3=frobeniusNorm(V3);
		double normV4=frobeniusNorm(V4);
		
		
		
		//need to normalize vectors
		for(int k=0;k<n;k++)
		{	
			if( normV1 != 0)
			{V1[k]=V1[k]/(normV1);}
			if( normV2 != 0)
			{V2[k]=V2[k]/(normV2);}
			if( normV3 !=0)
			{V3[k]=V3[k]/(normV3);}
			if( normV4 !=0)
			{V4[k]=V4[k]/(normV4);} //NORMALIZING
			
			
		}
		
		
		double d1=dotProduct(V1,V2);
		double d2=dotProduct(V2,V3);
		double d3=dotProduct(V3,V4);
		double d4=dotProduct(V4, V1);
		//dot prodcut of normalized vectors always in [-1,1]
		
		
		
		
		double as1= asin(d1);//domain of asin [-1,1]
		double as2= asin(d2);
		double as3= asin(d3);
		double as4= asin(d4);
		
		
		
		
		crossProduct(ra,rb,cross_P);
		for (int j=0;j<n;j+=1)
		{   
		  
		   aux1[j]=cross_P[j];
		   
		}
		
		double aux=dotProduct(aux1,r00);
		double alk=sign(aux)*(as1+as2+as3+as4)/(4*pi);
		 	
	
	return alk;
	
}

double compute_lk(std::vector<double> x, std::vector<double> y, std::vector<double> z, std::vector<double> x1, std::vector<double> y1, std::vector<double> z1, std::vector<double> m)
{//this is the linking number between the walk (x,y,z) and the walk (x1,y1,z1) translated by m
    	
    
	double lk=0;
	
	std::vector<double> x1t= vecAdd(x1, m[0]);
	std::vector<double> y1t= vecAdd(y1, m[1]);
	std::vector<double> z1t= vecAdd(z1, m[2]);
	std::vector<double> u1;
	std::vector<double> u2;
	

	for (int i=1;i<x1.size();i++) 
	{
		std::vector<double> u1{x[i-1],y[i-1],z[i-1]};
		
		std::vector<double> u2{x[i], y[i], z[i]};
		
			
		for(int j=1;j<x1t.size();j++)
		{
			
			
		 std::vector<double> v1{x1t[j-1],y1t[j-1],z1t[j-1]};	
		 std::vector<double> v2{x1t[j],y1t[j],z1t[j]};
			
		
			double glk=gauss_lk(u1,u2,v1,v2);
			
			lk=lk+glk;
			
		}
	
	 }

 return lk;

}


std::vector<std::vector<double>> compute_img(std::vector<double> &x1, std::vector<double> &y1, std::vector<double> &z1)
{
  std::vector<double> d1;
  std::vector<double> d2;
  std::vector<double> d3;
  std::vector<std::vector<double>> img; 
  img.push_back({0,0,0});
  d1.push_back(0);
  d2.push_back(0);
  d3.push_back(0);
  
  
  for(int j=1;j<400; j++)
  {
     double c1=round(x1[j-1]/(bxx)-0.5)*bxx;
     double c2=round(y1[j-1]/(bxy)-0.5)*bxy;
     double c3=round(z1[j-1]/(bxz)-0.5)*bxz;
  	
  
   if (abs(c1)==0.0)
    {       c1=0.0; }
   if (abs(c2)==0.0)
    {       c2=0.0;}
   if (abs(c3)==0.0) //abs in <cmath>
    {      c3=0.0 ;}
           
           
    std::vector<double> m{c1,c2,c3};
    
    
    bool isPresent=std::find(img.begin(), img.end(), m) !=img.end();//testing if m in img 
   
  
    if (isPresent == false)
    {
     img.push_back(m);
     d1.push_back(c1);
     d2.push_back(c2);
     d3.push_back(c3);
    
    }
    
  }
  

std::vector<std::vector<double>> D_all{d1,d2,d3};
/*print(d1);
std::cout<< "----END d1"<< std::endl;
print(d2);
std::cout<< "----END d2"<< std::endl;
print(d3);
std::cout<< "----END d3"<< std::endl;
*/
    return D_all;   
} 

double compute_lkScan(std::vector<double> &chainX, std::vector<double> &chainY, std::vector<double> &chainZ, std::vector<double> &chainX1, std::vector<double> &chainY1, std::vector<double> &chainZ1, std::map<std::pair<int,int>,double> lk_VALUES, std::vector<double> t1,double n_vertices1,double n_vertices2)
{       std::cout<< "computing lkscan: "<< std::endl;
	std::pair<int,int> indx_max;
	for (int i = 0; i<(400-n_vertices1); i+=50)
	{
		int start= i;
      		int finish= start+ n_vertices1;
	
		
	  std::vector<double> segx = slice(chainX,start,finish);
          std::vector<double> segy = slice(chainY,start,finish); //chain1 segment
          std::vector<double> segz = slice(chainZ,start,finish);
	
	
	
	 for ( int j=0; j<(400-n_vertices2); j+=10)
	 {
	 	
	 	//std::cout<<"starting monomers--- " << i << ","<< j << std::endl;
	 	int start1= j;
	 	int finish1= start1+n_vertices2;
	  std::vector<double> segx1 = slice(chainX1,start1,finish1);
          std::vector<double>  segy1 = slice(chainY1,start1,finish1); //chain2 segment
          std::vector<double>  segz1 = slice(chainZ1,start1,finish1);

	
	
	//print(segx1);
	//std::cout<< "----------" << std::endl; 
	
	
	
	std::pair<int,int> indx(i,j);
	
	
	
	double seg_max=compute_lk(segx,segy,segz,segx1,segy1,segz1,t1);// 2nd vector must be translated for images
	//need abs value of seg_max here
	lk_VALUES.insert({indx,abs(seg_max)});
	
	
	
	 }
	 
	}
	
	
	double max= map_MAX(lk_VALUES, indx_max) ; 
	
	
	std::fstream scan;
	scan.open ( "lkSCAN.txt", std::fstream::app);  
	
	
	//storing largest linking number value and associated segment length
	if (scan.is_open())
	{
	
	scan << max <<"\t" << indx_max.first << "," << indx_max.second<< "\t" << n_vertices1 << ","<< n_vertices2 << std::endl; 
	
	}
	
	else
	{
	  std::cout<< "Failed to open file. " << std::endl;   
	
	}
	
 return max;	
}	

double compute_lkp(std::vector<double> &ch1x,std::vector<double> &ch1y,std::vector<double> &ch1z, std::vector<double>&ch2x,std::vector<double>&ch2y,std::vector<double>&ch2z, std::vector<double> lkp_VALUES,double n_vertices1,double n_vertices2)
{
	std::vector<std::vector<double>> C_all;
	std::vector<std::vector<double>> D_all;
	std::map<std::pair<int,int>,double> lk_VALUES;
	std::pair<int,int> indx_max;
	std::vector<double> scanVALUES;	

	C_all=compute_img(ch1x,ch1y,ch1z);
	
	D_all=compute_img(ch2x,ch2y,ch2z);

	int m=C_all[0].size();
	int m1=D_all[0].size();
	
	 std::fstream stream;
	stream.open ( "LKPLOG.txt", std::fstream::app);  
	
	std::vector<std::vector<double>> trans; //translation vectors
	double lkp=0;
	double lk=0;
	double lkm=0;
	double tlk=0;
	double max_lk;
	double max_all_images;
	
	std::vector<double> lkl;
	

	
   for (int j=1; j<m+1;j++) //pay attention to end of iterations (m1-1)
   {	
   	for (int l=1;l<m1+1;l++)
   	{	
   		
   		double x1=C_all[0][j-1]-D_all[0][l-1]; 
   		double y1=C_all[1][j-1]-D_all[1][l-1]; 
   		double z1=C_all[2][j-1]-D_all[2][l-1];
   		double max_lk;
   		//print(ch2y);
		//std::cout<<"---End chain"<<std::endl;
   		
   		
   	
   		std::vector<double> t1{x1,y1,z1};
   		bool isPresent=std::find(trans.begin(), trans.end(), t1) !=trans.end();
   		if (isPresent == false )
   		
   		{
   		 trans.push_back(t1);
   		 /*std::vector<double> ch2xt = vecAdd(ch2x,t1[0]);
   		 std::vector<double> ch2yt = vecAdd(ch2y,t1[1]); // translated vectors
   		 std::vector<double> ch2zt = vecAdd(ch2z,t1[2]);
   		 */
   		 std::cout<< "---->" <<"t1: " << t1[0] << ", " << t1[1] << ", "<< t1[2]<< std::endl;
   		 lk=compute_lk( ch1x, ch1y, ch1z, ch2x, ch2y, ch2z,t1);
   		
   		/* (Need to automate plots)
   		
		std::fstream rings;
       	rings.open ( "RingsIMAGES.txt", std::fstream::app);  
	
	
		
			if (rings.is_open())
			{
			 for (int p = 0; p<400;p++){
			 rings << ch2xt << ch2yt << ch2z << //need to store as columns. 
			}
			}
			else
		  	{
	  		 std::cout<< "Failed to open file. " << std::endl;   
	
			}
   		
   		
   		
   		
   		
   		
   		*/
   		
   		 lkl.push_back(lk);
   		
   		
   		
   		
		 //print(ch2yt);
		 //std::cout<< "---END chain translation: "<< std::endl;
		 std::fstream scan;
		 scan.open ( "lkSCAN.txt", std::fstream::app);  
		 if (scan.is_open())
		 {
	
		 scan <<"(" << t1[0] <<","<< t1[1]<< "," << t1[2] << ")"<< std::endl; 
	
		 }
		
   		 max_lk=compute_lkScan( ch1x, ch1y, ch1z, ch2x, ch2y, ch2z,lk_VALUES,t1,n_vertices1, n_vertices2); 
   		
   		 
   		 scanVALUES.push_back(max_lk); 
   		
   		max_lk=*max_element(scanVALUES.begin(), scanVALUES.end());
   		
   		
   		 if ((abs(x1)==0.0) && (abs(y1)==0.0) && (abs(z1)==0.0))
   		 {	double lkg=lk;	}
   		 if (abs(lk)>0.0)
   		 { tlk=tlk+lk;           }
   		
   		 lkp=lkp+lk;
   		 lkp_VALUES.push_back(lkp); //appending to max_lk container
   		
   	        
   
   scan << "-----------------------------------------" <<std::endl;
        }
   
   }	
   }
   std::cout<< "scanVALUES: "<< std::endl; 
   print(scanVALUES);
   
   max_lk=*max_element(scanVALUES.begin(), scanVALUES.end());
   bool LINKING;
	
		if (abs(max_lk) >= 1)
		{
		  LINKING=1;  
	
		}
		else 
		{
		 LINKING=0;
	
		}
	
		std::ofstream stats;
        	stats.open("LinkingStats.txt", std::fstream::app);
		stats <<" " <<LINKING << " ";
			 
   std::cout<< std::endl;
   std::cout<< max_lk << std::endl;
   max_all_images=*max_element(lkp_VALUES.begin(), lkp_VALUES.end());
   lkm=*max_element(lkl.begin(), lkl.end()); //for maps not vectors? 
   
   
   std::cout<< std::endl << "lkm: " << lkm << std::endl;
   std::cout<< "lkp: " << lkp << std::endl;
   
   std::cout<< "max_all_images: " << max_all_images << std::endl;
   
  
	
	
	//storing largest linking number value and associated segment length
	if (stream.is_open())
	{
	
	//stream <<"# lkm, lkp, max_all_images, max_lk n_verticies"<<std::endl;
	stream << max_lk << "\t" << lkm << "\t" << lkp << "\t " << max_all_images <<"\t" << n_vertices1 << "," << n_vertices2 << std::endl; 
	
	}
	else
	{
	  std::cout<< "Failed to open file. " << std::endl;   
	
	}
	
 	
  
   //return (lkp, max_all_images); // "return maxx_all_images" 
   
   
   
   /* modification for lkp: 
   Return (lkp, max_lkp,image of chain 2, start of chain 1, end of chain 1, start of chain 2, end of chain 2) 
   */ 
   return lkp;
}

