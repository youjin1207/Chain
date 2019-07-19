#include <Rcpp.h>
#include <cmath>


using namespace Rcpp;
//using namespace RcppParallel;

//[[Rcpp::export]]
double sumC(NumericVector x) {
  int n = x.size();
  double total = 0;
  for(int i = 0; i < n; ++i) {
    total += x[i];
  }
  return total;
}


//[[Rcpp::export]]
double mainnine(NumericVector pars, NumericVector Y, NumericVector A, NumericVector X){
	
	// global variables in R
	// Environment env = Environment::global_env();
	IntegerVector idx1 = IntegerVector::create(0,1,2,3,4,5,6,7,8);
	
  	double  mainpart = sumC(Y[idx1]*pars[idx1]) +
  	  pars[9]*Y[0]*Y[2] + pars[10]*Y[0]*Y[3] + pars[11]*Y[0]*Y[4] + pars[12]*Y[0]*Y[6] +
     
      pars[13]*Y[1]*Y[5] + pars[14]*Y[1]*Y[7] + pars[15]*Y[1]*Y[8] + 
      pars[16]*Y[2]*Y[4] + pars[17]*Y[2]*Y[5] + pars[18]*Y[2]*Y[8] +
      pars[19]*Y[3]*Y[4] + pars[20]*Y[3]*Y[6] + 
      pars[21]*Y[4]*Y[5] + pars[22]*Y[4]*Y[6] + pars[23]*Y[4]*Y[7] + 

      pars[24]*Y[5]*Y[7] + pars[25]*Y[5]*Y[8] +
      pars[26]*Y[7]*Y[8] + 

      pars[27]*Y[0]*A[0] +
      pars[28]*Y[1]*A[1] + pars[29]*Y[2]*A[2] + pars[30]*Y[3]*A[3] + pars[31]*Y[4]*A[4] +
      pars[32]*Y[5]*A[5] + pars[33]*Y[6]*A[6] + pars[34]*Y[7]*A[7] + pars[35]*Y[8]*A[8] + 
      pars[36]*Y[0]*X[0] +
      pars[37]*Y[1]*X[1] + pars[38]*Y[2]*X[2] + pars[39]*Y[3]*X[3] + pars[40]*Y[4]*X[4] +
      pars[41]*Y[5]*X[5] + pars[42]*Y[6]*X[6] + pars[43]*Y[7]*X[7] + pars[44]*Y[8]*X[8]; 

	return(mainpart);
}

//[[Rcpp::export]]
double partition(NumericVector pars, NumericVector treatment, NumericVector covariate, NumericMatrix permutetab){
	// global variables in R
	// Environment env = Environment::global_env();
	// NumericMatrix permutetab = env["nine.permute"];

	double  part = 0;
  	for(int i=0; i < permutetab.nrow(); ++i){
  		part += exp(mainnine(pars, permutetab.row(i), treatment, covariate));
  	}

  	return(part);

}


// [[Rcpp::export]]
double loglikeninecov(NumericVector parameter, NumericMatrix inputY, 
  NumericMatrix inputA, NumericMatrix inputX, NumericMatrix permutetab){

	// global variables in R
	// Environment env = Environment::global_env();
	double whole = 0;
	double Zpart = 0;

	for(int i = 0; i < inputY.nrow(); ++i){
		whole += mainnine(parameter, inputY.row(i), inputA.row(i), inputX.row(i));
		Zpart += log(partition(parameter, inputA.row(i), inputX.row(i), permutetab));
	}
    
   
   	double negloglike = whole - Zpart;

   	return(-negloglike);
}


