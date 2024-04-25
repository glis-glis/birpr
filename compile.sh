#!/bin/bash

cd birp/src/
rm -rf coretools/
rm *.o
cd ../..


R CMD build birp
R CMD INSTALL birp
