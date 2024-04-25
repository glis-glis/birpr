
#include <Rcpp.h>
#include "coretools/Math/mathFunctions.h"
using namespace Rcpp;

// [[Rcpp::export]]
List rcpp_hello_world() {

    double fds = coretools::logistic(0);
    std::cout << fds << std::endl;

    double other = coretools::gammaLog(1.0);
    std::cout << other << std::endl;

    CharacterVector x = CharacterVector::create( "foo", "bar" )  ;
    NumericVector y   = NumericVector::create( 0.0, 1.0 ) ;
    List z            = List::create( x, y ) ;

    return z ;
}
