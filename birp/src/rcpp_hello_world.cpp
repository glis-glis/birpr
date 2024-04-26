
#include <Rcpp.h>
#include "coretools/Main/TMain.h"
#include "TBirpCore.h"
#include "coretools/Math/mathFunctions.h"
#include "stattools/MCMC/TMCMCFiles.h"

using namespace Rcpp;

// [[Rcpp::export]]
List birp_interface(List input) {
  
  // Create main by providing a program name, a version, an
  // affiliation, link to repo and contact email
  coretools::TMain main("Birp", "0.1", "University of Fribourg",
                        "https://bitbucket.org/wegmannlab/birp_cpp2",
                        "liam.singer@unifr.ch");
  
  // add existing tasks and tests
  main.addRegularTask("simulate", new TTask_simulate());
  main.addRegularTask("infer", new TTask_infer());
  
  // now run program
  // return main.run(input);
  
  return input;
}

// [[Rcpp::export]]
List rcpp_hello_world() {

    double fds = coretools::logistic(0);
    std::cout << fds << std::endl;

    double other = coretools::dummyTestRcpp(1.0);
    std::cout << other << std::endl;
    
    stattools::TStatePosteriorsReader reader("doesnotexist.txt");
    reader.close();

    CharacterVector x = CharacterVector::create( "foo", "bar" )  ;
    NumericVector y   = NumericVector::create( 0.0, 1.0 ) ;
    List z            = List::create( x, y ) ;

    return z ;
}
