
#include <Rcpp.h>
#include "coretools/Main/TMain.h"
#include "TBirpCore.h"

// [[Rcpp::export]]
Rcpp::List birp_interface(Rcpp::List Parameters, Rcpp::List Data) {
  
  // Create main by providing a program name, a version, an
  // affiliation, link to repo and contact email
  coretools::TMain main("Birp", "0.1", "University of Fribourg",
                        "https://bitbucket.org/wegmannlab/birpr",
                        "daniel.wegmann@unifr.ch");
  
  // add existing tasks and tests
  main.addRegularTask("simulate", new TTask_simulate());
  main.addRegularTask("infer", new TTask_infer());
  
  // now run program
  Rcpp::List result              = main.run(Parameters, Data);
  Rcpp::IntegerVector returnCode = result["returnCode"];
  
  if (returnCode[0] == 1) { // 1 encodes an error
    stop("Birp C++ terminated with errors");
  }
}
