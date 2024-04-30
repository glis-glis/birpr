
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
  return main.run(input);
}
