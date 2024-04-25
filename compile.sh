#!/bin/bash

cd birp/src/
rm -rf coretools/
rm -rf stattools/
rm -rf genometools/
rm -rf birp_cpp/
rm *.o
cd ../..


R CMD build birp
R CMD INSTALL birp
