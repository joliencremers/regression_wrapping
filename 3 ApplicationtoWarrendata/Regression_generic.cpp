//This code is written by Inge Jansen
//For WN sampler compile with -DWN
//Use like: ./Regression <nsum> <sigma_b> <inputfilename>
//Inputfilename should be formatted as follows:
//<string of arbitrary length (f.e. onepredictorWN)>_<number of samples>_<sample size>_<true values of parameters, separated by underscores>.dat

#include <string.h>
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <vector>
#include <algorithm>
#include <string>
#include <sstream>
#include <iterator>
#ifndef PI
#define PI (3.14159265358979323846f)
#endif
#include <sys/file.h>

//sigma_b is command line input
double sigma_b;
//used in prior of rho
double ap      = 0.5;

//Step-size in stepping out procedure (equal for al predictors)
double w_b = 1.0;

//Frequency of writing output to command line
int output_steps = 100;

//k in the WN approach
int    n_sum;

//n is the number of observations per sample, samples is the number of samples
//both are read from the filename of the input file
int    n;
int    samples;

//total number of iterations and burn-in
int    iters   = 7000;
int    burnin  = 3000;

FILE *output;
FILE *input;

//the predictor data (x1, x2,..) is stored in x, the outcome data is stored in y. iscategorical indicates whether a predictor
//is dichotomous.
std::vector<std::vector<double> > x;
std::vector<bool> iscategorical = std::vector<bool>();
std::vector<double> y;

//chain is a structure to store the current state of a sampler chain
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



//Function to assign start values to the chains
chain* initialize(int which){
	chain* ch = new chain();
	
	//startvalues in chain 1 are 0 for b's, and 0.1 for rho
	if(!which){
		ch->b0  = 0;
		for(int i=0;i<x.size();i++) {
			ch->b.push_back(0.0);
		}
		ch->rho = 0.1;		
	}
	 else{//startvalues in chain 2 are PI for b's and 0.9 for rho 
		ch->b0  = PI;
		for(int i=0;i<x.size();i++) {
			ch->b.push_back(PI);
		}
		ch->rho = 0.9;	
	}
	
	return ch;
}

//Function to read input data
chain* read_data(char * filename) {
    
	//The true values (in case of a simulation) are read from 
	//the filename and stored in a chain struct.
	chain* true_values = new chain();
	
	FILE* fp = fopen(filename, "r");

	//Determine how many predictors there are by counting the 
	//underscores in the filename:
	int numpredictors = std::count(filename, filename+strlen(filename), '_') - 4;
	printf("%d predictors detected\n",numpredictors);

	// split the filename by '_'
	std::vector<std::string> strparts = split(std::string(filename),'_');
	for(std::string s : strparts) {
		printf("input filename split contains part \"%s\"\n",s.c_str());
	}	
	//Determine number of samples, sample size and true parameter values
	samples = atoi(strparts[1].c_str());
	n = atoi(strparts[2].c_str());
	true_values->b0 = atof(strparts[3].c_str());
	int i=0;
	for(;i<numpredictors;i++) {
		//printf("trying to parse \"%s\"\n",strparts[4+i].c_str());
		true_values->b.push_back(atof(strparts[4+i].c_str()));
		//printf("part %d was read as: %f (from \"%s\")\n",i,true_values->b[i],strparts[4+i].c_str());
	}
	printf("i=%d\n",i);
	// the last strpart contains .dat which we need to take off
	//printf("the last strpart is: %s\n",strparts[4+i].c_str());
	
	std::string rhostr = strparts[4+i].substr(0,strparts[4+i].size()-4);
	//printf("the last strpart is: %s\n",rhostr.c_str());
	true_values->rho = atof(rhostr.c_str());

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

	//Read and store the input data
    for(int p = 0; p < (n*samples); ++p) {
    	//printf("reading line %d\n",p);
    	double tmp;
	    fscanf(fp, "%lf\t", &tmp);
	    y.push_back(tmp);
	    //printf("found y=%f\n",y[p]);
	    for(int i=0;i<x.size();i++) {
	    	double tmp;
	    	fscanf(fp,"%lf\t", &tmp);
	    	x[i].push_back(tmp);
	    	//printf("found x%d=%f\n",p,x[i][p]);
	    }
	}
    fclose(fp);
	
	return true_values;
}

//Function to calculate the joined distribution either based on WC (default) or WN (if compiled with -DWN)
double f(chain * ch, int p){
	
	double joint;
	double density_data = 1.0f;
	//The prior distribution of b0 is a uniform distribution, i.e. constant, and can be left out.
	//The prior distribution of continuous predictors are exponentials:
	double prior_b_arg  = 0.0;
	for(int i=0; i<ch->b.size(); i++) {
		if(!iscategorical[i]){prior_b_arg += ch->b[i]*ch->b[i];}
	}
	double prior_b = exp(-1.0f*prior_b_arg/(2.0f*(sigma_b*sigma_b)));
	
	//The prior distribution of sigma can be obtained from the prior of rho by substitution
	#ifdef WN
	double prior_sigma  = pow(ch->rho,ap)*pow(1.0f - ch->rho,ap-1.0f);
	#endif
	
	#ifdef WN
	//How sigma depends on rho:
	double sigma2 = -2.0f*log(ch->rho);
	
	//Calculation of the density of the data WN:
	for(int j=p; j<(p+n); j++){
		double sum_j = 0;
		double tmp = ch->b0;
		for(int i=0;i<x.size();i++) {
			tmp += ch->b[i] * x[i][j];
		}
		tmp = fmod(y[j] - tmp, 2.0*PI);
		
		for(int k=-n_sum; k<=n_sum; k++){
			double arg = -1.0f * (tmp  + 2.0f*PI*k) 
			                   * (tmp + 2.0f*PI*k) / (2.0f*sigma2);
			
			sum_j += exp(arg);
		}
		density_data *= sum_j;
	}

	double sigma = pow(sigma2, 0.5);
	//Now we can calculate the joint density by multiplying the above:
	joint = 1.0f/pow(sigma, -1.0f+(double)n) *density_data * prior_sigma;
	#else
	
	//Calculation of the density of the data WC:
	
	for(int j=p; j<(p+n); j++) {
		double tmp = ch->b0;
		for(int i=0;i<x.size();i++) {
			tmp += ch->b[i] * x[i][j];
		}
		tmp = fmod(y[j] - tmp, 2.0*PI);
		
		density_data *= 1 + ch->rho*ch->rho - 2*ch->rho*cos(tmp);
	}
	density_data = 1.0f/density_data;
	density_data *= pow(1-ch->rho,n)*pow(1+ch->rho,n);
	
	
	//Now we can calculate the joint density by multiplying the above:
	joint = density_data;
	#endif

	joint *= prior_b;

	return(joint);
}

//The "update" functions are used to update the model parameters according to the slice sampling rules

//Update circular intercept
void update_b0(chain * ch, int p){
	//generate a proposal value between 0 and 2PI
	ch->b0        = 2*PI*(double)rand()/RAND_MAX;
	//Check is the proposal is in the slice and (if yes) accept or (if no) sample a new proposal
	while(ch->d>=f(ch, p)){
		ch->b0    = 2*PI*(double)rand()/RAND_MAX;
	}
}

//update predictor parameters. Dichtomous predictors have a circular parameter, updating is along the way of circular intercept. Parameters of continuous predictors are updated using a stepping out procedure. 
void update_pr(chain * ch, int p, int predictor) {
	if(iscategorical[predictor-1]) {
		ch->b[predictor-1] = 2*PI*(double)rand()/RAND_MAX;
		
		while(ch->d>=f(ch, p)) {
			ch->b[predictor-1] = 2*PI*(double)rand()/RAND_MAX;
		}
	} else {
		//First place a interval of size w_b randomly around the current parameter value. 
		double b_old = ch->b[predictor-1];
		double w_l = ch->b[predictor-1] - w_b*(double)rand()/RAND_MAX;
		double w_r = w_l + w_b;
		
		//The interval is increased in steps of w_b until both sides are outside the slice
		ch->b[predictor-1] = w_l;
		while(ch->d<f(ch, p)){
			w_l -= w_b;
			ch->b[predictor-1]   = w_l; 
		} 
		ch->b[predictor-1] = w_r;
		while(ch->d<f(ch, p)){
			w_r += w_b;
			ch->b[predictor-1]   = w_r;
		}		
		//sample a random number uniformly from the interval
		ch->b[predictor-1] = (w_r-w_l)*(double)rand()/RAND_MAX + w_l;
		//if the proposal value is in the slice it is accepted, if not the interval is shrinked and a new proposal is sampled from the updated interval
		while(ch->d>=f(ch, p)){
			if(ch->b[predictor-1]<b_old) w_l = ch->b[predictor-1];
			if(ch->b[predictor-1]>b_old) w_r = ch->b[predictor-1];
			
			ch->b[predictor-1] = (w_r-w_l)*(double)rand()/RAND_MAX + w_l;
		}	
		// w_b = (w_r-w_l); // removed on 17-02-2017 to prevent interaction between chains
	}
}
//rho is updated by sampling uniformly between 0 and 1 
void update_rho(chain * ch, int p) {
	ch->rho  = (double)rand()/RAND_MAX;
	
	while(ch->d>=f(ch,p)){
		ch->rho  = (double)rand()/RAND_MAX;
	} 	
}

int main(int argc, char *argv[]){
	
	
	//setting the seed for the random number generator:
	srand(23);
	
	if(argc!=4){
		printf("use like this:\n./reg <nsum> <sigma_b> <filename>\n");
		return -1;
	}
	//n_sum (e.g., the wrapping coefficient k) and the prior sd are read from command line input
	n_sum        = atoi(argv[1]);
	sigma_b      = atof(argv[2]);
	
	printf("starting job with n_sum=%d\n",n_sum);
	
	//the filename should indicate the number of samples and the number of observations per sample folowed by 
	//the true values of the parameters
    //chain* true_values = read_data(argv[2]);
	read_data(argv[3]);
	
	//raw_output... will contain ALL output, output_foranalysis only contains values after burn-in
	char outputfilename1[150];
	char outputfilename2[150];
	sprintf(outputfilename1, "raw_output_%d_%d_%s",iters,n_sum,argv[3]);
	sprintf(outputfilename2, "output_foranalysis_%d_%d_%s",iters-burnin,n_sum,argv[3]);
	FILE* raw_output         = fopen(outputfilename1,"w+");
	FILE* output_foranalysis = fopen(outputfilename2,"w+");
	//This is where the actual sampler starts
	for(int sample_nr=0; sample_nr<samples; sample_nr++){
		int p = sample_nr*n;
		
		//initializing chains
		chain* ch1 = initialize(0);
		chain* ch2 = initialize(1);
		
		fprintf(raw_output, "%d\t%f\t%f\t", sample_nr, ch1->b0, ch2->b0);
		
		for(int i=0;i<x.size();i++) {
			fprintf(raw_output,"%f\t%f\t", ch1->b[i],ch2->b[i]);
		}
		fprintf(raw_output, "%f\t%f\r\n", ch1->rho,ch2->rho);
		
		
		for(int i=0; i<iters; i++){
			
			//update parameters:
			//sample auxiliary variable d uniformly from [0,f(current state)]
			ch1->d = f(ch1, p)*(double)rand()/RAND_MAX;
			ch2->d = f(ch2, p)*(double)rand()/RAND_MAX;
			
			update_b0(ch1, p);
			update_b0(ch2, p);

			for(int i=0;i<x.size();i++) {
				update_pr(ch1, p, i+1);
				update_pr(ch2, p, i+1);
			}
			
			update_rho(ch1, p);
			update_rho(ch2, p);
			
			//All data is written in the raw output file:
			fprintf(raw_output, "%d\t%f\t%f\t", sample_nr, ch1->b0, ch2->b0);
			for(int i=0;i<x.size();i++) {
				fprintf(raw_output,"%f\t%f\t", ch1->b[i],ch2->b[i]);
			}
			fprintf(raw_output, "%f\t%f\r\n", ch1->rho,ch2->rho);
			//After burn-in period, data is also written in the analysis file:
			if(i>burnin-1){
				fprintf(output_foranalysis, "%f\t%f\t", ch1->b0, ch2->b0);
				for(int i=0;i<x.size();i++) {
					fprintf(output_foranalysis,"%f\t%f\t", ch1->b[i],ch2->b[i]);
				}
				fprintf(output_foranalysis, "%f\t%f\r\n", ch1->rho,ch2->rho);
			}
			//Feedback printed to the screen:
			if(i % output_steps == 0){
				printf("\rsimulation %d: %.2f%%", sample_nr, (double)i/iters*100);
				fflush(stdout);
			}
		}

		printf("\rsimulation %d: 100.0%%\n", sample_nr);
	}
	fclose(raw_output);
	fclose(output_foranalysis);
	
	return(0);
}
