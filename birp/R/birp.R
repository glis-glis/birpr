#---------------------------------------
# R-package Birp
#---------------------------------------

#---------------------------------------
# Internal functions, not exported
#---------------------------------------

#' Function to convert an argument to a string and add it to a list if necessary
#' @param options A list where x should be added to
#' @param name A string specifying the name of the argument
#' @param x An R object to be added to the list
#' @return An updated list options
#' @keywords internal
.addToList.birp <- function(options, name, x){
  if (is.character(x)){
    options[[name]] <- x
  } else if (is.integer(x) | is.numeric(x)){
    options[[name]] <- paste0(x, collapse = ",")
  } else if (is.null(x)){
    # ignore, argument is empty
  } else if (is.na(x)){
    # ignore, argument is empty
  } else if (is.logical(x)){
    print(name)
    print(x)
    if (x){
      # is TRUE -> set flag for parameters().exists()
      options[[name]] <- ""
    } # else is FALSE -> ignore flag
  } else {
    stop(paste0("Unknown type ", class(x), " for name ", name, "!"))
  }
  return(options)
}

#' Function to add a hatched polygon to a plot
#' @param shading Shading color. If \code{NA}, shading is omitted
#' @param left An integer indicating the left-most value on the x-axis 
#' @param right An integer indicating the right-most value on the x-axis 
#' @return No return value, called for side effects.
#' @keywords internal
.plotShadingPolygon.birp <- function(shading, left, right){
  if (!is.na(shading)){
    x <- c(left, right, right, left, left)
    y <- par("usr")[c(3,3,4,4,3)]
    polygon(x, y, col = shading, border = NA, density = 20, angle = -45)
  }
}

#' Function to open an empty plot for plotting the posterior probabilities
#' @param xlim The x-limits (x1, x2) of the plot
#' @param ylim The y-limits (y1, y2) of the plot
#' @param xlab Name of x axis
#' @param ylab Name of y axis
#' @param shadingIncrease Shading color for the range gamma > 0. If \code{NA}, shading is omitted
#' @param shadingDecrease Shading color for the range gamma < 0. If \code{NA}, shading is omitted
#' @param lineAtZero If \code{TRUE}, adds a dashed line indicating 0.
#' @return No return value, called for side effects.
#' @keywords internal
.openPosteriorPlot.birp <- function(xlim, ylim,
                                    xlab, ylab,
                                    shadingIncrease, shadingDecrease,
                                    lineAtZero,
                                    ...){
  # Open plot
  plot(0, type = 'n', xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, ...)
  
  # Add shading polygons
  equalityCoordinate <- 0
  .plotShadingPolygon.birp(shadingIncrease, equalityCoordinate, par("usr")[2])
  .plotShadingPolygon.birp(shadingDecrease, par("usr")[1], equalityCoordinate)
  
  # Add line at gamma = 0
  if (lineAtZero){
    lines(rep(equalityCoordinate, 2), par("usr")[3:4], col = 'black', lty = 2)
  }
}

#' Function to generate a nice axis label with greek gamma and subscript
#' @param epoch The epoch in the subscript
#' @return A string
#' @keywords internal
.getLabelGamma.birp <- function(epoch){
  return(substitute(gamma[epoch], list(epoch=epoch)))
}

#' Function to add text box to plot denoting P(gamma > 0 | x) or P(gamma < 0 | x) for single epochs
#' @param x A birp object
#' @return No return value, called for side effects.
#' @keywords internal
.addTextSingleEpoch.birp <- function(x){
  diffFromBorder <- 0.01 * diff(par("usr")[1:2])
  if(x$prob_gamma_positive > 0.5){
    ttext <- bquote(paste("P(", gamma, " > 0 | D) = ", .(round(x$prob_gamma_positive, 3))))
    text(par("usr")[2] - diffFromBorder, par("usr")[4], adj = c(1, 1.5), labels = ttext)
  } else {
    ttext <- bquote(paste("P(", gamma, " < 0 | x) = ", .(round(1 - x$prob_gamma_positive, 3))))
    text(par("usr")[1] + diffFromBorder, par("usr")[4], adj = c(0, 1.5), labels = ttext)
  }
}

#' Function to add legend to plot denoting epochs
#' @param x A birp object
#' @param legend Add a legend to the plot
#' @param dens A list containing the densities for each epoch
#' @param xlim The x-limits (x1, x2) of the plot
#' @param col Line color, one per epoch
#' @param lwd Line width, one per epoch
#' @param lty Line type, one per epoch
#' @param ... additional parameters passed to the function.
#' @return No return value, called for side effects.
#' @keywords internal
.addLegendMultiEpoch.birp <- function(x, legend, dens, xlim, col, lwd, lty, ...){
  # Add legend
  # Check if highest density is left or right of plot
  max.y <- max(dens[[1]]$y)
  max.x <- dens[[1]]$x[dens[[1]]$y == max(dens[[1]]$y)]
  if (x$num_epochs > 1){
    for (e in 2:x$num_epochs){
      if (max(dens[[e]]$y) > max.y){
        max.y <- max(dens[[e]]$y)
        max.x <- dens[[e]]$x[dens[[e]]$y == max(dens[[e]]$y)]
      }
    }
  }
  
  if (max.x < xlim[1] + diff(xlim)/2){
    legend.pos <- 'topright'
  } else {
    legend.pos <- 'topleft'
  }
  
  legend(legend.pos, legend, col = col, lwd = lwd, lty = lty, ...)
}

#' Function to get the start of each epoch, including for a hypothetical epoch after the last epoch
#' @param times An integer or numeric vector with time points
#' @param times_of_change An integer or numeric vector with times of change
#' @return An integer or numeric vector with starting points for each epoch
#' @keywords internal
.getEpochStarts.birp <- function(times, times_of_change){
  return(c(0, times_of_change, max(times[length(times)], times_of_change + 1)))
}

#' Function to calculate rho 
#' @param times An integer or numeric vector with time points
#' @param times_of_change An integer or numeric vector with times of change
#' @return A numeric matrix containing rho for each time point and epoch
#' @keywords internal
.calculateRho.birp <- function(times, times_of_change){
  epoch_start_T <- .getEpochStarts.birp(times, times_of_change)
  num_epochs <- length(times_of_change) + 1
  rho <- matrix(0, nrow = length(times), ncol = num_epochs)
  for (e in 1:num_epochs){
    rho[times >= epoch_start_T[e+1], e] <- epoch_start_T[e+1] - epoch_start_T[e]
    within_epoch <- times < epoch_start_T[e+1] & times > epoch_start_T[e]
    rho[within_epoch ,e] <- times[within_epoch] - epoch_start_T[e]
  }
  return(rho)
}

#---------------------------------------
# Constructor
#---------------------------------------

#' Creating a Birp Object
#'
#' This function creates a birp object by running the MCMC
#' @param data A birp_data object
#' @param timesOfChange A numeric or integer vector specifying the times of change
#' @param negativeBinomial A boolean indicating if Poisson (default) or negative binomial model should be used
#' @param stochastic A boolean indicating if deterministic (default) or stochastic trend model should be used
#' @param assumeTrueDetectionProbability A boolean indicating if provided detection probabilities are "true", i.e. meaning that they will be transform to logit and not standardized
#' @param prefixOutputCommandLine The prefix provided to command-line birp. Used to locate output files
#' @return An object of class birp
#' @examples 
#' b <- birp()
#' @export
birp <- function(data,
                 timesOfChange = c(),
                 negativeBinomial = FALSE,
                 stochastic = FALSE,
                 assumeTrueDetectionProbability = FALSE,
                 prefixOutputCommandLine = NA,
                 iterations = 100000,
                 numBurnin = 10,
                 burnin = 1000,
                 thinning = 10
                 ){
  # Check for valid arguments
  stopifnot(class(data) == "birp_data")
  
  # Create named list of function arguments 
  args <- c(as.list(environment()))
  
  # Get temporary directory where output will be written
  if (is.na(prefixOutputCommandLine)){
    out <- tempfile()
  } else {
    out <- prefixOutputCommandLine
  }

  # Parse options and convert to string
  options <- list(task = "infer", out = out)
  for (i in 1:length(args)){
    if (names(args)[i] == "data") next # skip data: no command-line argument
    options <- .addToList.birp(options, names(args)[i], args[[i]])
  }
  
  # Add input data names
  options[["data"]] <- paste(data$method_names, collapse = ",")
  
  # Run MCMC
  if (is.na(prefixOutputCommandLine)){
    birp::birp_interface(options, data$data)
  }

  # Read output files
  meanVar <- read.table(paste0(out, "_meanVar.txt"), header = T)
  trace <- read.table(paste0(out, "_trace.txt"), header = T)
  gamma <- read.table(paste0(out, "_gammaSummaries.txt"), header = T)
  timepoints <- read.table(paste0(out, "_timepoints.txt"), header = T)$timepoints
  
  # Calculate statistics on gamma
  gamma_posterior_mean <- meanVar$posterior_mean[grepl("gamma", meanVar$name)]
  trace_gamma <- as.matrix(trace[,grepl("gamma", names(trace))])
  gamma_posterior_median <- apply(trace_gamma, 2, median)
  gamma_posterior_q05 <- apply(trace_gamma, 2, quantile, probs=0.05)
  gamma_posterior_q95 <- apply(trace_gamma, 2, quantile, probs=0.95)
  matrix_gamma <- as.matrix(gamma[,2:ncol(gamma)])
  prob_gamma_positive <- diag(matrix_gamma)
  
  # Define results
  x <- list(data = data,
            options = options,
            meanVar = meanVar,
            trace = trace,
            trace_gamma = trace_gamma,
            gamma = gamma,
            num_epochs = length(timesOfChange) + 1,
            times_of_change = timesOfChange,
            gamma_posterior_mean = gamma_posterior_mean,
            gamma_posterior_median = gamma_posterior_median,
            gamma_posterior_q05 = gamma_posterior_q05,
            gamma_posterior_q95 = gamma_posterior_q95,
            prob_gamma_positive = prob_gamma_positive,
            timepoints = timepoints
            )
  class(x) <- "birp"
  
  return(x)
}

#---------------------------------------
# Methods for printing
#---------------------------------------

#' Printing a birp object
#'
#' @param x A birp object.
#' @param ... Additional parameters passed to print functions.
#' @return No return value, called for side effects.
#'
#' @export
#' @seealso \code{\link{birp}}
#' @examples
#' b <- birp()
#' print(b)
#' 

print.birp <- function(x, ...){
  cat("Birp estimates:\n")
  if (x$num_epochs > 1){ # only for print multi-epoch
    cat(" - times of change: [", paste0(x$times_of_change, collapse = ", "), "]\n", sep = "")
  }
  cat(" - Posterior mean of gamma: [", paste0(x$gamma_posterior_mean, collapse=", "), "]\n", sep = "")
  cat(" - Posterior median of gamma: [", paste0(x$gamma_posterior_median, collapse=", "), "]\n", sep = "")
  cat(" - Posterior 5% quantile of gamma: [", paste0(x$gamma_posterior_q05, collapse=", "), "]\n", sep = "")
  cat(" - Posterior 95% quantile of gamma: [", paste0(x$gamma_posterior_q95, collapse=", "), "]\n", sep = "")
  cat(" - Posterior probability of increasing trend P(gamma > 0): [", paste0(x$prob_gamma_positive, collapse=", "), "]\n", sep = "")
  invisible(x)
}

#' Summarizing a birp object
#'
#' @param object A birp object.
#' @param ... Additional parameters passed to summary functions.
#' @return No return value, called for side effects.
#'
#' @export
#' @seealso \code{\link{bexy}}
#' @examples
#' b <- birp()
#' print(b)
summary.birp <- function(object, ...){
  print.birp(object, ...)
}

#---------------------------------------
# Methods for plotting
#---------------------------------------

#'  Plotting a birp object
#'
#' @param x A birp object
#' @param shadingIncrease Shading color for the range gamma > 0. If \code{NA}, shading is omitted
#' @param shadingDecrease Shading color for the range gamma < 0. If \code{NA}, shading is omitted
#' @param col Line color, one per epoch. If a single value is provided, it is recycled to match the number of epochs.
#' @param lwd Line width, one per epoch. If a single value is provided, it is recycled to match the number of epochs.
#' @param lty Line type, one per epoch. If a single value is provided, it is recycled to match the number of epochs.
#' @param xlim The x-limits (x1, x2) of the plot. If NA, these are determined automatically
#' @param ylim The y-limits (y1, y2) of the plot. If NA, these are determined automatically
#' @param add If \code{TRUE}, posterior density is added to currently open plot. If FALSE, a new plot is opened.
#' @param xlab Name of x axis
#' @param ylab Name of y axis
#' @param legend Add a legend to the plot. Use NA to suppress
#' @param lineAtZero If \code{TRUE}, adds a dashed line indicating 0.
#' @param ... additional parameters passed to the function.
#' @return No return value, called for side effects.
#'
#' @export
#' @seealso \code{\link{birp}}
#' @examples 
#' b <- birp()
#' plot(b)
plot.birp <- function(x,
                      shadingIncrease = NA,
                      shadingDecrease = "#f2c7c7",
                      col = "black",
                      lwd = 1,
                      lty = 1:x$num_epochs,
                      xlim = NA,
                      ylim = NA,
                      add = FALSE,
                      xlab = expression(gamma),
                      ylab = "Posterior density",
                      legend = paste("Epoch", 1:x$num_epochs),
                      lineAtZero = TRUE,
                      ...){
  # Recycle col, lwd and lty
  col <- rep_len(col, x$num_epochs)
  lwd <- rep_len(lwd, x$num_epochs)
  lty <- rep_len(lty, x$num_epochs)
  
  # Calculate all densities
  dens <- list(x$num_epochs)
  for (e in 1:x$num_epochs){
    dens[[e]] <- stats::density(x$trace_gamma[,e])
  }
  
  # Get limits
  if (any(is.na(xlim))){
    xlim <- range(sapply(dens, function(d) range(d$x)))
  }
  if (any(is.na(ylim))){
    ylim <- range(sapply(dens, function(d) range(d$y)))
  }
  
  # Open plot
  if (!add){
    .openPosteriorPlot.birp(xlim, ylim, xlab, ylab, shadingIncrease, shadingDecrease, lineAtZero, ...)
  }
  
  # Plot densities
  for (e in 1:x$num_epochs){
    lines(dens[[e]], col = col[e], lwd = lwd[e], lty = lty[e], ...)
  }
  
 
  # Add legend?
  if (!any(is.na(legend))){
    if (x$num_epochs == 1){
      .addTextSingleEpoch.birp(x)
    } else {
      .addLegendMultiEpoch.birp(x, legend, dens, xlim, col, lwd, lty, ...)
    }
  }
}

#' Plotting posterior estimates of epoch pairs
#'
#' @param x A birp object
#' @param epoch1 The index of the first epoch to plot
#' @param epoch2 The index of the second epoch to plot
#' @param xlab A label for the x axis
#' @param ylab A label for the y axis
#' @param xlim The x-limits (x1, x2) of the plot. Note that x1 > x2 is allowed and leads to a "reversed axis". The default value, NULL, indicates that the range of the finite values to be plotted should be used
#' @param ylim The y-limits of the plot
#' @param col The color for the contour lines
#' @param diag.col The color of the diagonal line. Use NA to indicate that no line should be plotted
#' @param diag.lwd The line width of the diagonal line
#' @param diag.lty The line type of the diagonal line
#' @param zero.col The color of the line at zero. Use NA to indicate that no line should be plotted
#' @param zero.lwd The line width of the line at zero
#' @param zero.lty The line type of the line at zero
#' @param print.p If \code{TRUE}, add text representing the posterior probability of a trend change
#' @param ... additional parameters passed to the function
#' @return No return value, called for side effects.
#'
#' @export
#' @seealso \code{\link{birp}}
#' @examples 
#' b <- birp()
#' plot_epoch_pair(b)
plot_epoch_pair <- function(x, 
                            epoch1 = 1,
                            epoch2 = 2,
                            xlab = .getLabelGamma.birp(epoch1),
                            ylab = .getLabelGamma.birp(epoch2),
                            xlim = range(x$trace_gamma[,c(epoch1, epoch2)]),
                            ylim = xlim,
                            col = "deeppink",
                            diag.col = "black",
                            diag.lwd = 1,
                            diag.lty = 1,
                            zero.col = "black",
                            zero.lwd = 1,
                            zero.lty = 2,
                            print.p = TRUE,
                            add = FALSE,
                            ...){
  # check if x has at least 2 epochs
  if (x$num_epochs < 2) {
    stop("Need at least 2 epochs!")
  }
  
  # Check parameters
  if (epoch1 < 1 | epoch1 > x$num_epochs){ stop("Epoch ", epoch1, " does not exist!") }
  if (epoch2 < 1 | epoch2 > x$num_epochs){ stop("Epoch ", epoch2, " does not exist!") }
  
  # Obtain density estimates
  dens <- MASS::kde2d(x$trace_gamma[,epoch1], x$trace_gamma[,epoch2])
  
  #make 2D density plot
  contour(dens$x, dens$y, dens$z, 
          xlim = xlim, ylim = ylim, 
          col = col, 
          xlab = xlab, ylab = ylab, 
          add = add,
          ...)
  
  # Add diagonal
  if (!add & !is.na(diag.lwd) & diag.lwd > 0){
    abline(0, 1, col = diag.col, lwd = diag.lwd, lty = diag.lty)
  }
  
  # Add lines at zero
  if (!add & !is.na(zero.lwd) & zero.lwd > 0){
    if (xlim[1] <= 0 & xlim[2] >= 0){
      lines(c(0, 0), par("usr")[3:4], col = zero.col, lwd = zero.lwd, lty = zero.lty, ...)
    }
    if (ylim[1] <= 0 & ylim[2] >= 0){
      lines(par("usr")[1:2], c(0, 0), col = zero.col, lwd = zero.lwd, lty = zero.lty, ...)
    }
  }
  
  # Print P(gamma.epoch1 < gamma.epoch2)
  q <- sum(x$trace_gamma[,epoch1] < x$trace_gamma[,epoch2]) / nrow(x$trace_gamma)
  
  if (!add & print.p){
    if (q < 0.5){
      text(par("usr")[1] + 0.005 * diff(par("usr")[1:2]), 
           par("usr")[4] - 0.03 * diff(par("usr")[3:4]), 
           pos = 4, 
           labels = substitute(
               paste('P(', gamma[epoch1], ' < ', gamma[epoch2], ' | x) = ', q),
               list(epoch1 = epoch1, epoch2 = epoch2, q = round(q, digits=4))))
    } else {
      text(par("usr")[2] - 0.005 * diff(par("usr")[1:2]), 
           par("usr")[3] + 0.03 * diff(par("usr")[3:4]), 
           pos = 2, 
           labels = substitute(
             paste('P(', gamma[epoch1], ' > ', gamma[epoch2], ' | x) = ', q),
             list(epoch1 = epoch1, epoch2 = epoch2, q = round(1 - q, digits=4))))
    }
  }
}

#' Plotting posterior trajectories
#'
#' @param x A birp object
#' @param xlim The x limits (x1, x2) of the plot. Note that x1 > x2 is allowed and leads to a "reversed axis". The default value, NULL, indicates that the range of the finite values to be plotted should be used.
#' @param n_points Number of points
#' @param quantiles Which quantiles to plot
#' @param quantile.col Colors of the quantiles
#' @param quantile.border Define border of the quantile. NA is possible
#' @param median.col Color of the median
#' @param median.lwd Line width of median
#' @param median.lty Line type of median
#' @param epoch.col Color to represent the epochs
#' @param epoch.lwd Line width to represent the epochs
#' @param epoch.lty Line type to represent the epochs
#' @param times.col Color to represent the times of change
#' @param times.lwd Line width that represents times of change
#' @param times.lty Line type that represents times of change.
#' @param log Plot relative densities in log
#' @param xlab A label for the x axis
#' @param ylab A label for the y axis
#' @param ... additional parameters passed to the function
#' @return No return value, called for side effects
#'
#' @export
#' @seealso \code{\link{birp}}
#' @importFrom grDevices gray
#' @examples 
#' b <- birp()
#' plot_trajectory(b)

plot_trajectory <- function(x, 
                            xlim = range(x$timepoints), 
                            n_points = 1000, 
                            quantiles = c(0.99, 0.9, 0.5, 0.25), 
                            quantile.col = "gray"(seq(1, 0, length.out = length(quantiles)+2)[2:(length(quantiles)+1)]), 
                            quantile.border = NA,
                            median.col = "deeppink",
                            median.lwd = 1,
                            median.lty = 1,
                            epoch.col = "black",
                            epoch.lwd = 1,
                            epoch.lty = 1,
                            times.col = "black",
                            times.lwd = 1,
                            times.lty = 2,
                            log = FALSE,
                            xlab = "Time",
                            ylab = paste(c("log", "Relative Density")[c(log, TRUE)], collapse=" "),
                            ...){
  # Check parameters
  if(max(quantiles) > 1.0){ stop("Provided quantiles must be <= 1.0!") }
  if(min(quantiles) <= 0.0){ stop("Provided quantiles must be > 0.0!") }
  if(sum(xlim < 0) > 0){ stop("xlim range must be positive!") }
  if(xlim[2] <= xlim[1]){ stop("xlim range is zero or inverted!") }
  
  # Prepare calculations of means
  epoch_ranges <- c(xlim[1], x$times_of_change[x$times_of_change > xlim[1] & x$times_of_change < xlim[2]], xlim[2])
  epoch_length <- epoch_ranges[2:length(epoch_ranges)] - epoch_ranges[1:(length(epoch_ranges)-1)]
  rho <- .calculateRho.birp(epoch_ranges, x$times_of_change)
  num_epochs <- length(epoch_length)
  
  # Which epochs are relevant?
  if (x$num_epochs == 1){
    first.gamma.col <- 1
  } else if(xlim[1] >= max(x$times_of_change)){
    first.gamma.col <- length(x$times_of_change) + 1
  } else {
    first.gamma.col <- min(which(c(x$times_of_change > xlim[1])))
  }
  if (x$num_epochs == 1){
    last.gamma.col <- 1
  } else if(xlim[2] <= min(x$times_of_change)){
    last.gamma.col <- 1
  } else {
    last.gamma.col <- max(which(x$times_of_change < epoch_ranges[length(epoch_ranges)]))+1
  }
  gamma.cols <- first.gamma.col:last.gamma.col
  
  # Prepare points at which to calculate rates
  xvals <- seq(xlim[1], xlim[2], length.out = n_points)
  rho_x <- .calculateRho.birp(xvals, x$times_of_change)
  mcmc_length <- nrow(x$trace)
  rates <- matrix(0, ncol = length(xvals), nrow = mcmc_length - 1)
  
  for (i in 2:mcmc_length){
    # Calculate mean to normalize / align
    # Prevent underflow by normalizing with mean
    change <- rho %*% x$trace_gamma[i,]
    meanLog <- mean(change)
    change <- exp(change - meanLog)
    average <- sum((change[2:(num_epochs+1),1] - change[1:num_epochs]) / x$trace_gamma[i,gamma.cols] / epoch_length)
    
    # Calc normalized rates
    rates[i-1,] <- rho_x %*% x$trace_gamma[i,] - log(average) - meanLog
  }
  
  if(!log){
    rates <- exp(rates)
  }
  
  # Calculate quantiles and median of distribution
  probs <- sort(c((1-quantiles)/2, 0.5, 1-(1-quantiles)/2))
  quant <- apply(rates, 2, quantile, probs = probs)
  
  # Open plot
  plot(0, type = 'n', xlim = xlim, ylim = range(quant, na.rm = TRUE), xlab = xlab, ylab = ylab, ...)
  
  # Add epochs
  if(!is.na(epoch.lwd) & epoch.lwd>0){
    for(t in x$times_of_change[x$times_of_change > xlim[1] & x$times_of_change < xlim[2]]){
      lines(rep(t, 2), par("usr")[3:4], col = epoch.col, lwd = epoch.lwd, lty = epoch.lty)
    }
  }
  
  # Add times with measurements
  if(!is.na(times.lwd) & times.lwd>0){
    for(t in x$timepoints[x$timepoints >= xlim[1] & x$timepoints <= xlim[2]]){
      lines(rep(t, 2), par("usr")[3:4], col = times.col, lwd = times.lwd, lty = times.lty)
    }
  }
  
  # Plot quantiles
  n_probs <- length(probs)
  x_poly <- c(xvals, rev(xvals), xvals[1])
  for(q in 1:length(quantiles)){
    y_poly <- c(quant[q,], rev(quant[n_probs-q+1,]), quant[q,1])
    polygon(x_poly, y_poly, col = quantile.col[q], border = quantile.border)
  }
  
  # Plot median
  if (!is.na(median.lwd) & median.lwd > 0){
    lines(xvals, quant[length(quantiles) + 1,], lwd = median.lwd, lty = median.lty, col = median.col)
  }
}

#' Plotting the MCMC chains
#'
#' @param x A birp object
#' @param col Color(s) used in the plot
#'
#' @return No return value, called for side effects
#' @export
#' @seealso \code{\link{birp}}
#' @examples 
#' b <- birp()
#' plot_mcmc(b)
plot_mcmc <- function(x, col=c("black", "blue")){
  # Layout
  layout(matrix(1:(2*x$num_epochs), ncol = 2, byrow=TRUE), widths = c(2,1))
  
  # Plot MCMC and posterior for each epoch
  mcmc_len <- nrow(x$trace)
  
  for(e in 1:x$num_epochs){
    # Plot trace
    xax <- 1:nrow(x$trace_gamma) * as.numeric(x$options$thinning)
    plot(xax, x$trace_gamma[,e], xlab = "Iteration", ylab = bquote(gamma[.(e)]))

    # Plot density
    plot(stats::density(x$trace_gamma[,e]),  main="", xlab=bquote(gamma[.(e)]), ylab="Posterior density")
  }
}



