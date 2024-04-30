#---------------------------------------
# R-package Birp
#---------------------------------------

#' Creating a Birp Data Object
#'
#' This function creates a birp_data object that is used to estimate trends in Poisson rates with birp().
#' @param counts an m x n matrix of the observed counts. Each of the m rows corresponds to a data set obtained at each of n times (columns).
#' @param efforts an m x n matrix of the effort conducted to observe the counts in data.
#' @param times a vector giving the n time points at which counts were obtained.
#' @param data.set.names names to distinguish your data sets. 
#' @return An object of type birp_data
#' @examples 
#' dat <- birp_data(c(10,20,30), c(100,200,300), c(1,2,5));
#' @export
birp_data <- function(counts, efforts, times, data.set.names=paste("Data set", 1:dim(counts)[1])){
  #check variables
  if(sum(is.na(counts)) > 0) stop("Counts can not be NA! Put zero in case no counts were observed.")
  if(sum(is.na(efforts)) > 0) stop("Efforts can not be NA! Put zero in case of no effort.")
  if(sum(counts < 0) > 0 || sum(efforts<0) > 0)  stop("Counts and efforts can not be negative!")
  if(sum(times < 0) > 0) stop("Time points can not be negative!");
  if(sum(efforts==0 & counts>0)>0) stop("Counts can not be > 0 if effort = 0!");
  
  #check dimensionality
  if(is.vector(counts)){
    counts <- matrix(counts, nrow=1);
  }
  if(!is.matrix(counts)){
    counts <- as.matrix(counts);
  }
  if(is.vector(efforts)){
    efforts <- matrix(efforts, nrow=1);
  }
  if(!is.matrix(efforts)){
    efforts <- as.matrix(efforts);
  }
  if(sum(dim(counts) == dim(efforts))!=2) stop("Counts and efforts must have the same dimensionality!");
  
  #remove data sets with less than two time points with data (effort > 0)
  noEfforts <- which(rowSums(efforts > 0) < 2);
  if(length(noEfforts) > 0){
    warning(paste("Removing data sets", paste(noEfforts, collapse=", "), " due to less than two time points with effort > 0."));
    if(length(noEfforts) == dim(efforts)[1]) stop("No data set with sufficient effort!");
    
    counts <- counts[-noEfforts,];
    efforts <- efforts[-noEfforts,];
  }
  
  #create class
  value <- list(num.time.points=length(times),
                num.data.sets=dim(counts)[1],
                counts=counts,
                efforts=efforts,
                data.set.names=data.set.names,
                times=times);
  attr(value, "class") <- "birp_data";
  value;
}