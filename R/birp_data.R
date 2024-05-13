#---------------------------------------
# R-package Birp
#---------------------------------------

#---------------------------------------
# Internal functions, not exported
#---------------------------------------

#' This function check if a birp_data object is valid
#' @param data A list of dataframes to validate
#' @return No return value, called for side effects.
#' @keywords internal
.validate.birp_data <- function(data){
  if (!is.list(data) | !all(sapply(data, is.data.frame))){
    stop("Argument 'data' of constructor 'birp_data' should be a single data frame or a list of data frames!")
  }
  
  # Check data frames (most checks happen in C++, only check minimum here)
  for (i in 1:length(data)){
    if (ncol(data[[i]]) < 4) stop("Data frame should have at least four columns!")
    
    if (!("timepoint" %in% names(data[[i]]))) stop("Data frame should contain a column with name 'timepoint'")
    if (!("location" %in% names(data[[i]]))) stop("Data frame should contain a column with name 'location'")
    if (!any(grepl("counts", names(data[[i]])))) stop("Data frame should contain a column starting with 'counts'")
    if (!any(grepl("covEffort", names(data[[i]])))) stop("Data frame should contain a column starting with 'covEffort'")
  }
}

#---------------------------------------
# Constructor
#---------------------------------------

#' Creating a Birp Data Object based on dataframe(s)
#'
#' This function creates a birp_data object
#' @param data A single dataframe or a list of data frames (one per method)
#' @return An object of type birp_data
#' @examples
#' b <- new_birp_data()
#' @export
new_birp_data <- function(data){
  if (is.data.frame(data)){ data <- list(data) }
  
  .validate.birp_data(data)
  
  # Get method names
  method_names <- names(data)
  if (is.null(method_names)){
    method_names <- paste0("Method_", 1:length(data))
  }
  
  # Get times and locations
  times <- sort(unique(unlist(sapply(data, function(x) x$timepoint))))
  locations <- sort(unique(unlist(sapply(data, function(x) x$location))))
  
  # Get number of data sets (location-method-combination)
  num_data_sets <- sum(sapply(data, function(x) length(unique(x$location))))
  
  x <- list(data = data,
            method_names = method_names,
            times = times,
            locations = locations,
            num_data_sets = num_data_sets
            )
  class(x) <- "birp_data"
  
  return(x)
}

#---------------------------------------
# Constructor Helpers
#---------------------------------------

#' Creating a Birp Data Object based on counts and efforts for a single method
#'
#' This function creates a birp_data object
#' @param counts An J x K matrix of the observed counts. Each of the J rows corresponds to a location obtained at each of K times (columns)
#' @param efforts An J x K matrix of the effort conducted to observe the counts
#' @param times A vector giving the K time points at which counts were obtained
#' @param location_names Names to distinguish the locations
#' @return An object of type birp_data
#' @examples 
#' b <- birp_data(c(10,20,30), c(100,200,300), c(1,2,5))
#' @export
birp_data <- function(counts, efforts, times, location_names = paste0("Location_", 1:nrow(counts))){
  # Check variables
  if (any(counts < 0, na.rm = T) | any(efforts < 0, na.rm = T))  stop("Counts and efforts can not be negative!")
  if (any(is.na(times))) stop("NA in times are not allowed!")
  if (any(times < 0)) stop("Time points can not be negative!")
  if (any(efforts == 0 & counts > 0)) stop("Counts can not be > 0 if effort = 0!")
  
  # Check dimensionality
  if (is.vector(counts)){
    counts <- matrix(counts, nrow = 1)
  }
  if (!is.matrix(counts)){
    counts <- as.matrix(counts)
  }
  if (is.vector(efforts)){
    efforts <- matrix(efforts, nrow = 1)
  }
  if (!is.matrix(efforts)){
    efforts <- as.matrix(efforts)
  }
  if (sum(dim(counts) == dim(efforts)) != 2) stop("Counts and efforts must have the same dimensionality!")
  if (length(location_names) != nrow(counts)) stop("Length of location_names must match the number of rows of counts!")
  if (length(times) != ncol(counts)) stop("Length of times must match the number of columns of counts!")
  
  # Create data frame
  dat <- data.frame()
  for (j in 1:nrow(counts)){
    for (k in 1:ncol(counts)){
      if (is.na(counts[j,k]) | is.na(efforts[j,k])){ next; } # ignore counts or efforts with NA
      dat <- rbind(dat, c(location_names[j], times[k], counts[j,k], efforts[j,k]))
    }
  }
  names(dat) <- c("location", "timepoint", "counts", "covEffort")
  
  # call constructor of birp_data
  b <- new_birp_data(dat)
  
  return(b)
}

#' Creating a Birp Data Object based on filenames
#'
#' This function creates a birp_data object
#' @param filenames A vector of filenames specifying the input file(s) (one per method)
#' @param method_names Names to distinguish the methods. If NA, method names will be derived from filenames
#' @param sep The field separator character
#' @return An object of type birp_data
#' @examples 
#' b <- birp_data(c(10,20,30), c(100,200,300), c(1,2,5))
#' @export
birp_data_from_file <- function(filenames, method_names = NA, sep = ""){
  if (!is.na(method_names) & length(method_names) != length(filenames)){
     stop("Number of method names must match the number of filenames!")
  }
  
  # Check if filenames is actually a "masterfile" containing other filenames
  if (length(filenames) == 1){
    f <- read.table(filenames[1], sep = sep, header = FALSE)
    if (ncol(f) == 1){
      filenames <- f[,1]
      tryCatch({
        f <- read.table(filenames[1], sep = sep, header = TRUE)
      }, warning = function(e){
        stop("Failed to correctly parse input file! Please make sure the correct separator ('sep') is used.")
      }, error = function(e) {
        stop("Failed to correctly parse input file! Please make sure the correct separator ('sep') is used.")
      })
    }
  }
  
  # Read files
  dat <- list()
  for (i in 1:length(filenames)){
    f <- read.table(filenames[i], sep = sep, header = TRUE)
    # Get method name from filename
    if (!is.na(method_names)){
      method_name <- method_names[i]
    } else {
      tmp <- strsplit(filenames[i], "/")[[1]]
      tmp <- tmp[length(tmp)]
      tmp <- strsplit(tmp, "\\.")[[1]]
      method_name <- paste(tmp[-length(tmp)], collapse = "_")
      if (method_name %in% names(dat)){
        stop("Method names obtained from filenames are not unique! Please provide as extra argument 'method_names'.")
      } 
    }
    # Store in list
    dat[[method_name]] <- f
  }
  
  # call constructor of birp_data
  b <- new_birp_data(dat)
  
  return(b)
}

#' This function simulates a birp_data object
#' @param filenames A vector of filenames specifying the input file(s) (one per method)
#' @return An object of type birp_data
#' @examples 
#' b <- birp_data(c(10,20,30), c(100,200,300), c(1,2,5))
#' @export
simulate_birp <- function(){
}

#---------------------------------------
# Printing birp_data
#---------------------------------------

#' Printing a birp_data Object
#' @param x The birp_data object to be printed.
#' @param ... other parameters
#' @examples 
#' dat <- birp_data(c(10,20,30), c(100,200,300), c(1,2,5))
#' print(dat)
#' @export
print.birp_data <- function(x, ...){
  cat("birp_data object for", length(x$method_names), "method(s),", length(x$locations),"location(s) and", length(x$times),"time points:\n");
  cat(" - methods: [", paste(x$method_names, collapse=", "), "]\n", sep="");
  cat(" - locations: [", paste(x$locations, collapse=", "), "]\n", sep="");
  cat(" - time points: [", paste(x$times, collapse=", "), "]\n", sep="");
  cat(" - total number of data points: ", sum(sapply(data, length)) ,"\n", sep="");
}

#' This function summarizes a birp_data object
#' @param x The birp_data object to be printed.
#' @param ... Other parameters
#' @examples 
#' dat <- birp_data(c(10,20,30), c(100,200,300), c(1,2,5))
#' summary(dat)
#' @export
summary.birp_data <- function(x, ...){
  print.birp_data(x);
}

#---------------------------------------
# Plotting birp_data
#---------------------------------------

#' Plotting a birp_data Object
#'
#' This function plots MLE estimates of rate (lambda) estimates per time-point, method and location
#' @param x The birp data object to be printed.
#' @param col A vector of colors, recycled to match the number of methods and locations
#' @param lwd A vector of line width, recycled to match the number of methods and locations
#' @param lty A vector of line types, recycled to match the number of methods and locations
#' @param xlab The label of the x-axis
#' @param ylab The label of the y-axis
#' @param legend.x The x coordinate to position the legend. Use legend.x=NA to omit legend
#' @param legend.y The y coordinate to position the legend
#' @param legend.bty The type of box to be drawn around the legend. The allowed values are "o" (the default) and "n".
#' @param xlim Set the limits of the x-axis
#' @param ylim Set the limits of the y-axis
#' @param ... Additional parameters passed to plotting functions.
#' @examples 
#' dat <- birp_data(c(10,20,30), c(100,200,300), c(1,2,5));
#' plot(dat);
#' @export
plot.birp_data <- function(x, 
                           col = 1:x$num_data_sets,
                           lwd = 1,
                           lty = 1,
                           xlab = "time",
                           ylab = "lambda",
                           legend.x = "topright",
                           legend.y = NULL,
                           legend.bty = "o",
                           xlim = range(x$times), 
                           ylim = NA, 
                           ...){
  col <- rep_len(col, x$num_data_sets)
  lwd <- rep_len(lwd, x$num_data_sets)
  lty <- rep_len(lty, x$num_data_sets)
  
  # Estimate rates: counts / efforts per method-location combination
  rates <- list()
  names <- numeric(x$num_data_sets)
  counter <- 1
  # Loop over all methods
  for (i in 1:length(x$data)){
    method <- x$data[[i]]
    loc <- unique(sort(method$location))
    ix_counts <- which(grepl("counts", names(method)))
    ix_effort <- which(grepl("covEffort", names(method)))
    if (length(ix_counts) != 1) stop("Only support plotting birp_data for single-species (single column)!")
    if (length(ix_effort) != 1) stop("Only support plotting birp_data when effort is fixed (single column)!")
    # Loop over all locations for current method
    for (j in 1:length(loc)){
      # Get all data for this method-location combination
      method_loc <- method[method$location == loc[j],]
      # Only keep non-zero efforts
      keep <- method_loc[,ix_effort] > 0
      counts <- method_loc[keep, ix_counts]
      efforts <- method_loc[keep, ix_effort]
      times <- method_loc$timepoint[keep]
      # Store rate, timepoints and name
      rates[[counter]] <- list(rates = counts / efforts, times = times)
      names[counter] <- paste(x$method_names[i], loc[i], sep = "_")
      counter <- counter + 1
    }
  }
  
  if (is.na(ylim)){ ylim <- range(unlist(sapply(rates, function(x) x$rates))) }
  
  # Open plot
  plot(0, type='n', xlim = xlim, xlab = xlab, ylab = ylab, ylim = ylim, ...)
  for (i in 1:x$num_data_sets){
    lines(rates[[i]]$times, rates[[i]]$rates, col = col[i], lwd = lwd[i], lty = lty[i], type = 'b', ...)
  }  
  
  # Add legend
  if(!is.na(legend.x)){
    legend(x = legend.x, y = legend.y, 
           bty = legend.bty, 
           legend = names, 
           lwd = lwd, lty = lty, col = col)
  }
}
