//This code is written by Inge Jansen
//Works for both WN and WC

#include <stdio.h>
#include <iostream>
#include <ios>
#include <iomanip>
#include <sstream>
#include <fstream>
#include <algorithm>
#include <vector>
#include <string>
#include <iterator>
#include <cstring>
#include <ctime>
#include <math.h>
#include <stdlib.h>
#ifndef PI
#define PI (3.14159265358979323846f)
#endif
#include <sys/file.h>
#include <cassert>

using std::endl;
using std::cout;

int autocor;           //boolean indicating if autocorrelations were requested
int iters;             //number of iterations on every sample (excl burnin)
int nsum;              //nr of times the distribution was wrapped in the simulation
int samples;           //nr of samples in simulation
int n;                 //nr of observations per sample
int numpredictors;     //number of predictors in model

//The summary structure contains the mean and sd. The meaning for circular variables is slightly different: mean is circular mean and sd refers to rho (sorry, might be bit confusing)
typedef  struct {
	double sd;  
	double mean;
	
} summary;

//The quantiles structure contains the lower and upper bound of the 95%CI and the coverage probability
typedef struct {
	double lower;
	double upper;
	double CP;
} quantiles;

//defining vectors to store data
std::vector<std::vector<double>> x;
std::vector<bool> iscategorical = std::vector<bool>();
std::vector<double> y;

struct chain {
	double b0;
	std::vector<double> b;
	double rho;
	double d;
	chain() {
		b = std::vector<double>();
    }
};

typedef struct chain chain;

std::vector<double> b0;
std::vector<std::vector<double>> b;
std::vector<double> rho;

//Functionality to split strings. Thanks to: http://stackoverflow.com/questions/236129/split-a-string-in-c 
template<typename Out>
void split(const std::string &s, char delim, Out result) {
    std::stringstream ss;
    ss.str(s);
    std::string item;
    while (std::getline(ss, item, delim)) {
        *(result++) = item;
    }
}
std::vector<std::string> split(const std::string &s, char delim) {
    std::vector<std::string> elems;
    split(s, delim, std::back_inserter(elems));
    return elems;
}

//Function to read the data (both sampling results and original input data)
chain* read_data(char * filename){
	
	chain* true_values = new chain();//chain* true_values = (chain*)malloc(sizeof(chain));
	
	FILE* fp = fopen(filename, "r");

	//Determine how many predictors there are:
	numpredictors = std::count(filename, filename+strlen(filename), '_') - 8;
	printf("%d predictors detected\n",numpredictors);

	// split the filename by '_'
	std::vector<std::string> strparts = split(std::string(filename),'_');
	for(std::string s : strparts) {
		printf("input filename split contains part \"%s\"\n",s.c_str());
	}	
	
	iters = atoi(strparts[2].c_str());
	nsum  = atoi(strparts[3].c_str());
	samples = atoi(strparts[5].c_str());
	n = atoi(strparts[6].c_str());
	true_values->b0 = atof(strparts[7].c_str());
	int i=0;
	for(;i<numpredictors;i++) {
		printf("trying to parse \"%s\"\n",strparts[8+i].c_str());
		true_values->b.push_back(atof(strparts[8+i].c_str()));
		printf("part %d was read as: %f (from \"%s\")\n",i,true_values->b[i],strparts[8+i].c_str());
	}
	printf("i=%d\n",i);
	// the last strpart contains .dat which we need to take off
	printf("the last strpart is: %s\n",strparts[8+i].c_str());
	
	std::string rhostr = strparts[8+i].substr(0,strparts[8+i].size()-4);
	printf("the last strpart is: %s\n",rhostr.c_str());
	true_values->rho = atof(rhostr.c_str());

	//Read parameter data:
	std::fstream fin(filename);
	b0.resize(iters*samples*2);
	rho.resize(iters*samples*2);
	b.resize(numpredictors);
	for(int j=0;j<numpredictors;j++) {
		b[j].resize(iters*samples*2);
	}
	
    
    for(int p = 0; p < (iters*samples); ++p){
        fin >> b0[p]; //fscanf(fp, "%lf\t", &b0[p]);
        fin >> b0[p+iters*samples]; //fscanf(fp, "%lf\t", &b0[p+iters*samples]);

        for(int j=0;j<numpredictors;j++) {
        	fin >> b[j][p]; //fscanf(fp, "%lf\t", &b[j][p]);
        	fin >> b[j][p+iters*samples]; //fscanf(fp, "%lf\t", &b[j][p+iters*samples]);
        		
        }
        
		fin >> rho[p]; //fscanf(fp, "%lf\t", &rho[p]);
        fin >> rho[p+iters*samples]; //fscanf(fp, "%lf\t", &rho[p+iters*samples]);
	
		if(p % 100 == 0){
            printf("\rreading data: %.2f%%", (double)p/(iters*samples)*100);
            fflush(stdout);
        }
	}
	printf("\rreading data: 100.0%%\r\n");
    fin.close();
	
		
	//Read original input data:
	
	// char inputfile[150];
	std::stringstream inputfilename;
    
	inputfilename <<  strparts[4].c_str() 
			      << "_" << samples
			      << "_" << n
			      << "_" << std::fixed <<  std::setprecision(1) << true_values->b0;

	for(i=0;i<numpredictors;i++){
		inputfilename << "_" << std::fixed <<  std::setprecision(1) << true_values->b[i];
	}
	
	inputfilename << "_" << std::fixed <<  std::setprecision(1) << true_values->rho << ".dat";
	
	fp = fopen(inputfilename.str().c_str(), "r");	
	printf("Open file:%s\r\n", inputfilename.str().c_str());
	
	// parse and read first line to decide the type of each predictor
	double tmp;
	// skip outcome
	fscanf(fp, "%lf\t", &tmp);
	for(int i=0;i<numpredictors;i++) {
		fscanf(fp,"%lf\t", &tmp);
		iscategorical.push_back(tmp==0 || tmp==1);
		std::vector<double> xi;
		x.push_back(xi);
		printf("found predictor %lf of type %s\n",tmp,iscategorical[i]?"dichotomous":"continuous");
	}
	// reset fp to start of file
	rewind(fp); 
	
	for(int p = 0; p < (n*samples); ++p) {
    	//printf("reading line %d\n",p);
    	double tmp;
	    fscanf(fp, "%lf\t", &tmp);
	    y.push_back(tmp);
	    //printf("found y=%f\n",y[p]);
	    for(int i=0;i<numpredictors;i++) {
	    	double tmp;
	    	fscanf(fp,"%lf\t", &tmp);
	    	x[i].push_back(tmp);
	    	//printf("found x%d=%f\n",p,x[i][p]);
	    }
	}
    fclose(fp);
	
	return true_values;
}


//Function to calculate circular means. Returning the circular dispersion rho and the circular mean. 
//start is needed when the mean of some subset of data needs to be calculated
summary* mean_circular(std::vector<double> data, int length_of_data, int repetitions, int start) {
	summary* mean = new summary;
	
	double x_total = 0.0;
	double y_total = 0.0;
	double rho_total = 0.0;
	
	for(int j=0; j<repetitions; j++){
		double x = 0.0;
		double y = 0.0;
		//calculate horizontal and vertical components
		for(int i=0; i<length_of_data; i++){
			x += cos(data[i+j*length_of_data+start]);
			y += sin(data[i+j*length_of_data+start]);
		}
	
		x_total += x;
		y_total += y;
		//rho equals the mean resultant length
		rho_total += sqrt(x*x + y*y)/length_of_data;  
	
	
	}
	//the mean is defined as the direction of the vector sum
	mean->mean = atan2(y_total,x_total);
	mean->sd       = rho_total/repetitions;	
	
	
	return(mean);
}

//function to calculate linear mean and sd, optionally of a subset of the data
summary* mean_linear(std::vector<double> data, int length_of_data, int repetitions, int start){
	summary* s = new summary;
	
	double mean  = 0.0;
	double sd    = 0.0;
	
	
	for(int j=0; j<repetitions; j++){
		double sample_mean = 0.0;
		double sample_sd   = 0.0;
	
		//first we calculate the sample mean
		for(int i=0; i<length_of_data; i++){
			sample_mean += data[i+j*length_of_data+start];
		}
		sample_mean /= length_of_data;
		
		//then we calculate the sample sd
		for(int k=0; k<length_of_data; k++){
			sample_sd += (data[k+j*length_of_data+start]-sample_mean)*(data[k+j*length_of_data+start]-sample_mean);
		}
			
		sample_sd /= (length_of_data-1);
		
		mean += sample_mean;
		sd   += sqrt(sample_sd);
	}
	
	s->mean = mean/repetitions;
	s->sd   = sd/repetitions;	
	
	return(s);
}

//calculation of linear 95%CI and coverage probability. Data needs to be sorted. 
quantiles* quantiles_linear(std::vector<double> data, int length_of_data, int repetitions, double true_value){
	double q2_5  = 0.0;
	double q97_5 = 0.0;
	double CP    = 0.0;
	
	for(int i=0;i<repetitions; i++){
		//std::sort(data+i*length_of_data,data+length_of_data*(i+1));
		std::sort(data.begin()+i*length_of_data,data.begin()+length_of_data*(i+1));
		// std::sort(i*length_of_data,length_of_data*(i+1));
		double q2_5_temp  = data[i*length_of_data + (int)((length_of_data)*0.025)];
		double q97_5_temp = data[i*length_of_data + (int)((length_of_data)*0.975)];
		q2_5  += q2_5_temp;
		q97_5 += q97_5_temp;
		CP    += 1.0f*(true_value>q2_5_temp)*(true_value<q97_5_temp);
	}
	quantiles* q = new quantiles;
	q->lower = q2_5 /repetitions;
	q->upper = q97_5/repetitions;
	q->CP    = CP   /repetitions;
	
	return(q);
}

//Function to calculate circular 95%CI and CP
//The list is sorted with respect to the mean-pi (i.e. the point on the circel opposite to the mean)
quantiles* quantiles_circular(std::vector<double> data, int length_of_data, int repetitions, double true_value){
	double q2_5_x  = 0.0;
	double q97_5_x = 0.0;
	double q2_5_y  = 0.0;
	double q97_5_y = 0.0;
	double CP      = 0.0;
	
	for(int i=0;i<repetitions; i++){
		//first, we calculate the mean of the current sample
		double x = 0.0;
		double y = 0.0;
		for(int j=0; j<length_of_data; j++) {
			x += cos(data[j+i*length_of_data]);
			y += sin(data[j+i*length_of_data]);
		}
	
		double mean = atan2(y,x);
		//then we sort the current sample with respect to mean-PI
		std::sort(data.begin()+i*length_of_data,data.begin()+length_of_data*(i+1),[=](double j, double k)->int {
			return fmod(j-mean+PI*3.0,2*PI)<fmod(k-mean+PI*3.0,2*PI);
			});
		// std::sort(i*length_of_data,length_of_data*(i+1),[=](double j, double k)->int {
			// return fmod(j-mean+PI*3.0,2*PI)<fmod(k-mean+PI*3.0,2*PI);
			// });
		
		//we pick the quantiles from the sorted list
		//std::cout << "2.5% quantile located at " << (int)((length_of_data)*0.025) << "has value";
		double e1 = data[i*length_of_data + (int)((length_of_data)*0.025)];
		//std::cout << e1 << std::endl;
		//std::cout << "97.5% quantile located at " << (int)((length_of_data)*0.975) << "has value";
		double e3 = data[i*length_of_data + (int)((length_of_data)*0.975)];
		//std::cout << e3 << std::endl;
		
		//and add to the x and y component
		q2_5_x  += cos(e1);
		q2_5_y  += sin(e1);
		q97_5_x += cos(e3);
		q97_5_y += sin(e3);
		
		
		int tmp = (e3>e1)*(true_value>e1)*(true_value<e3) + (e1>e3)*((true_value>e1)+(true_value<e3));
		assert(tmp==0.0 || tmp==1.0);
		CP    += tmp;
	}
	quantiles* q = new quantiles;
	q->lower = fmod(PI*2+atan2(q2_5_y,q2_5_x),PI*2);
	q->upper = fmod(PI*2+atan2(q97_5_y,q97_5_x),PI*2);
	q->CP    = CP/repetitions;
	return(q);
}

//The two functions below are an implementation of Bickel et al. (2006) to calculate the mode.

//This function searches for an interval [x[j],x[j+s-1]] of size s
//in the data that minimises x[j+s-1]-x[j] and returns j. n is the 
//length of the array x. The data in x must be sorted!
int smalest_interval_of_size(double x[], int n, int s)
{
	int j=0;								// index of the current minimum
	double wmin;							// the current minimum
	wmin = x[n-1] - x[0];					// full range is a safe upper bound 
	for(int i=0;i<=n-s;i++)					// the last possible i is x[n-s] because then x[i+s-1]=x[n-s+s-1]=x[n-1] (x[n] doesn't exist)
	{
		double w = x[i+s-1]-x[i];	// width of this range
		if(w < wmin)						
		{
			wmin = w;
			j = i;
		}
	}
	return j;
}


//This funtion calculates the half sample mode by recursively finding smalest intervals of half the size.
//x is the list of datapoints and n is the size of that list.
double half_sample_mode(double x[], int n)
{
	// there are two possible initial cases, n==1 or n==2:
	if(n==1)	return x[0];
	// when n==2 the mode is defined as the average of the two
	if(n==2)	return (x[0]+x[0])/2.0;
	
	// otherwise recurse:
	int intervalsize = ceil(0.5*n);
	return half_sample_mode(x+smalest_interval_of_size(x,n,intervalsize),intervalsize);
}
//Function to calculate the mode averaged over the samples 
double mean_mode(double data[], int length_of_data, int repetitions){
	double res = 0.0;
	
	for(int i=0; i<repetitions; i++){
		std::sort(data+i*length_of_data,data+length_of_data*(i+1));
		res += half_sample_mode(data+length_of_data*i,length_of_data);
	}
	return res/repetitions;
}
//Function to calculate Highest posterior Density interval and CP. Used for parameter rho.
quantiles HPD(double data[], int length_of_data, int repetitions, double true_value){
	double lower = 0.0;
	double upper = 0.0;
	double CP    = 0.0;
	
	int lower_i;
	int s = 0.95*length_of_data;
	
	for(int i=0; i<repetitions; i++){
		lower_i = smalest_interval_of_size(data+i*length_of_data,length_of_data,s);
		lower += data[i*length_of_data+lower_i];
		upper += data[i*length_of_data+lower_i+s-1];
		CP    += (true_value>data[i*length_of_data+lower_i]) * (true_value<data[i*length_of_data+lower_i+s-1]);
 	}
	
	
	quantiles q;
	q.lower = lower/repetitions;
	q.upper = upper/repetitions;
	q.CP    = CP/repetitions;
	
	return q;
}

//This function can be used to calculates autocorrelation for observations that are a maximum of 100 iterations away from each other.
void autocorrelation(chain* true_values, char * filename){
	
	char aufilename[255];
	std::string filestr = std::string(filename).substr(26,strlen(filename));
	sprintf(aufilename,"autocor_%d_%d_%s",iters,nsum,filestr.c_str());
	
	
    FILE* au = fopen(aufilename,"w+");
	
	int dt_steps = 1;
	for(int i=0; i<(2*samples); i++){
		summary* b0_mean   = mean_circular(b0,  iters, 1, i*iters);
		std::vector<summary*> summaries;
		for(int j=0;j<numpredictors;j++) {
			if(iscategorical[j]) {
				summaries.push_back(mean_circular  (b[j],  iters, 1, i*iters));
			} else {
				summaries.push_back(mean_linear  (b[j],  iters, 1, i*iters));
			}
		}
		summary* rho_mean  = mean_linear  (rho, iters, 1, i*iters);
		
		double b0_denom = b0_mean->sd*iters;

		std::vector<double> denoms;
		for(int j=0;j<numpredictors;j++) {
			denoms.push_back(summaries[j]->sd*summaries[j]->sd*iters);
		}
		double rho_denom = rho_mean->sd*rho_mean->sd*iters;
		
		for(int dt=1; dt<101; dt+=dt_steps){
			double b0_nom   = 0.0;
			
			std::vector<double> noms;
			for(int j=0;j<numpredictors;j++) {
				noms.push_back(0.0);
			}

			double rho_nom   = 0.0;
	
			for(int t=0; t<(iters-dt); t++) {
				b0_nom  += (fmod(b0[i*iters + t]-b0_mean->mean,2*PI))*(fmod(b0[i*iters + t + dt]-b0_mean->mean,2*PI));
				
				for(int j=0;j<numpredictors;j++) {
					if(iscategorical[j]) {
						noms[j] += (fmod(b[j][i*iters + t]-summaries[j]->mean,2*PI))*(fmod(b[j][i*iters + t + dt]-summaries[j]->mean,2*PI));				
					} else {
						noms[j] += (b[j][i*iters + t]-summaries[j]->mean)           *     (b[j][i*iters + t + dt]-summaries[j]->mean);	
					}
				}
				
				rho_nom +=      (rho[i*iters + t]-rho_mean->mean)         *     (rho[i*iters + t + dt]-rho_mean->mean);
				
			}
	
			double b0_au     = b0_nom/b0_denom;

			std::vector<double> b_au;
			for(int j=0;j<numpredictors;j++) {
				b_au.push_back(noms[j]/denoms[j]);
			}
			double rho_au    = rho_nom/rho_denom;
			
			fprintf(au, "%d\t%f\t", dt, b0_au);
			for(int j=0;j<numpredictors;j++) {
				fprintf(au,"%f\t",b_au[j]);
			}
			fprintf(au, "%f\r\n", rho_au);
			
		}
//		printf("\rCalculating autocorrelation: %f%%", (double)i/(2*samples)*100);
        cout << "\rCalulating autocorrelation: " << (double)i/(2*samples)*100 << "%" << std::flush;
//		fflush(stdout);
		
	}
	
	printf("\rCalculating autocorrelation: 100.0%%         \n");
	fclose(au);
	
}


int main(int argc, char *argv[]){


	if(argc<3||argc>4){
		printf("use like this:\n./analysis <autocor> <filename> [analysisfilename]\n");
		return -1;
    }

	//Determine if autocorrelations are requested
	autocor        = atoi(argv[1]);
		
	//Read data
	chain* true_values = read_data(argv[2]);
		
	//calulate descriptives of the data
	std::vector<summary*> mean_x;

	summary* mean_y   = mean_circular(y, n, samples, 0);
	for(int i=0;i<numpredictors;i++){
		
		mean_x.push_back(mean_linear(x[i], n, samples, 0));
		
	}
	
	//Calculate autocorrelations if requested. This needs to be done on unsorted data! This means we need to do it first as the quantile functions will sort the data.
    if(autocor) autocorrelation(true_values, argv[2]);
	
	//calculate simulation results:
	std::vector<summary*> mean_b;
	std::vector<quantiles*> q_b;

	printf("\rAnalysing b0");
    fflush(stdout);
	summary*    mean_b0 = mean_circular(b0, iters, 2*samples, 0);
	quantiles* q_b0    = quantiles_circular(b0, iters, 2*samples, true_values->b0);
	for(int i=0;i<numpredictors;i++){
		printf("\rAnalysing b%d",i+1);
		fflush(stdout);
		if(iscategorical[i]){
			mean_b.push_back(mean_circular(b[i], iters, 2*samples, 0));
			q_b.push_back(quantiles_circular(b[i], iters, 2*samples, true_values->b[i]));
		}
		else{
			mean_b.push_back(mean_linear(b[i], iters, 2*samples, 0));
			q_b.push_back(quantiles_linear(b[i], iters, 2*samples, true_values->b[i]));
		}
	}
	
	printf("\rAnalysing rho");
    fflush(stdout);
    double * rho_as_list = (double *)malloc(sizeof(double)*rho.size());
    for(unsigned int i=0;i<rho.size();i++) {
    	rho_as_list[i] = rho[i];
    }
	double mode_rho   = mean_mode(rho_as_list, iters, 2*samples); 
	quantiles q_rho   = HPD(rho_as_list, iters, 2*samples, true_values->rho);
	

	
	printf("\rAnalysing done\r\n");
	
	char outputfilename[255];
	std::string filestr = std::string(argv[2]).substr(26,strlen(argv[2])-4);
	sprintf(outputfilename,"analysis_%d_%d_%s.txt",iters,nsum,filestr.c_str());

	
	FILE * outputfile;
	outputfile = fopen(outputfilename,"w");
	
	//Writing analysis output
	fprintf(outputfile,"This is the output of CLM v2.0\r\nCopyright by Inge Jansen\r\n");
	char datetext[255];
	time_t rawtime;
	struct tm * timeinfo;
	time(&rawtime);
	timeinfo = localtime(&rawtime);
	strftime (datetext,255,"%c",timeinfo);
	fprintf(outputfile,"%s\r\n\r\n",datetext);
	fprintf(outputfile,"A simulation was performed on a dataset with %d samples,\r\n", samples);
	fprintf(outputfile,"each sample consisting of %d observations.\r\n\r\n", n);
	fprintf(outputfile,"The simulation output can be found in: %s.\r\n\r\n", argv[2]);

	fprintf(outputfile,"The dataset contained observations on %d variables:\r\n", numpredictors+1);
	fprintf(outputfile,"a circular outcome variable y, and predictors");
    for(int i=0;i<numpredictors;i++){
		if(iscategorical[i]) {fprintf(outputfile,"x%d (dichotomous), ", i+1);}
		else {fprintf(outputfile,"x%d (continuous)", i+1);}
    }
    fprintf(outputfile, ".\r\n");
	
	fprintf(outputfile,"The data were simulated with parameters b0=%.1f", true_values->b0);
	for(int i=0;i<numpredictors;i++){
		fprintf(outputfile,", b%d=%.1f",i+1, true_values->b[i]);
	}
	fprintf(outputfile," and rho=%.1f\r\n\r\n",true_values->rho);
	
	fprintf(outputfile,"Descriptive statistics of the input data:\r\n");
	fprintf(outputfile,"%10s %14s %14s\r\n", " ","mean","sd");
	fprintf(outputfile,"%10s %14f %14f\r\n", "y",  mean_y->mean, sqrt(-2*log(mean_y->sd)));
	for(int i=0; i<numpredictors;i++){
		char prname[3];
		sprintf(prname,"x%d",i+1);
		fprintf(outputfile,"%10s %14f %14f\r\n", prname, mean_x[i]->mean, mean_x[i]->sd);
	}
	
	fprintf(outputfile,"\r\n");
	
	fprintf(outputfile,"The simulation was performed using 2 chains and %d iterations per sample (excluding burnin).\r\n",iters);
	
	fprintf(outputfile,"Simulation results:\r\n");
	fprintf(outputfile,"%11s %14s %14s %14s %14s %14s %14s\r\n", "","mean", "mode", "sd","lower","upper", "CP") ;
	fprintf(outputfile,"%11s %14f %14s %14f %14f %14f %14f\r\n", "b0", mean_b0->mean, "",sqrt(-2*log(mean_b0->sd)), q_b0->lower, q_b0->upper, q_b0->CP);
	for(int j=0;j<numpredictors;j++) {
		char parname[3];
		sprintf(parname,"b%d",j+1);
		fprintf(outputfile,"%11s %14f %14s %14f %14f %14f %14f\r\n", parname, mean_b[j]->mean,  "", mean_b[j]->sd, q_b[j]->lower, q_b[j]->upper, q_b[j]->CP);	
	}
	fprintf(outputfile,"%11s %14s %14f %14s %14f %14f %14f\r\n", "rho", "", mode_rho,    "", q_rho.lower, q_rho.upper, q_rho.CP);
	
	
	if(autocor) {
		char aufilename[255];
		std::string aufilestr = std::string(argv[2]).substr(26,strlen(argv[2]));
		sprintf(aufilename,"autocor_%d_%d_%s",iters,nsum,aufilestr.c_str());
		fprintf(outputfile, "\r\n\r\nAutocorrelations can be found in %s\r\n", aufilename);
	}
	
	fclose(outputfile);
	
	//Writing analysis ouput to a analysis file, not updated for generic yet.
	if(argc==4){
		FILE * a_out;
		a_out = fopen(argv[3],"a+");
		
		//fprintf(a_out,"%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\r\n",
		//                                           "b0_true","b1_true","b2_true","cor_true","rho_true",
		//                                           "b_b0","b_b1","b_b2","b_cor","b_rho",
		//											 "rb_b0","rb_b1","rb_b2","rb_cor","rb_rho",
		//											 "sd_b0","sd_b1","sd_b2",
		//											 "b_cp_b0","b_cp_b1","b_cp_b2","b_cp_cor","b_cp_rho",
		//											 "R2",
		//											 "iw_b0","iw_b1","iw_b2","iw_cor","iw_rho",
		//											 "k","burnin","iters")
		fprintf(a_out,"%f\t%f\t%f\t%f\t", true_values->b0, true_values->b[1], true_values->b[2],true_values->rho); //True values
		fprintf(a_out,"%f\t%f\t%f\t%f\t", (mean_b0->mean-true_values->b0),(mean_b[1]->mean-true_values->b[1]), (mean_b[2]->mean-true_values->b[2]), (mode_rho-true_values->rho)); //Bias of estimator
		fprintf(a_out,"%f\t%f\t%f\t%f\t", (mean_b0->mean-true_values->b0)/true_values->b0,(mean_b[1]->mean-true_values->b[1])/true_values->b[1], (mean_b[2]->mean-true_values->b[2])/true_values->b[2],(mode_rho-true_values->rho)/true_values->rho); //Relative bias of estimator
		fprintf(a_out,"%f\t%f\t%f\t",sqrt(-2*log(mean_b0->sd)),iscategorical[0]?(sqrt(-2*log(mean_b[1]->sd))):(mean_b[1]->sd), iscategorical[1]?(sqrt(-2*log(mean_b[2]->sd))):(mean_b[2]->sd)); //standard deviations
		fprintf(a_out,"%f\t%f\t%f\t%f\t", q_b0->CP, q_b[1]->CP, q_b[2]->CP,q_rho.CP); //Bias of CP
		fprintf(a_out,"%f\t%f\t%f\t%f\t", q_b0->upper-q_b0->lower,q_b[1]->upper-q_b[1]->lower,q_b[2]->upper-q_b[2]->lower,q_rho.upper-q_rho.lower);//Interval widths
		fprintf(a_out,"%d\r\n",nsum);
		
		fclose(a_out);
	
	}
    return 0;
}


