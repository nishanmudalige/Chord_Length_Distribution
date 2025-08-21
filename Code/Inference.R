#####################################################
## Functions for generating points on hypersphere ###
#####################################################

# Generate uniform points on hyper sphere
# Based on the work by Muller (1959) and Marsaglia (1972)
# hypersphere_uniform_points = function(n, dim, radius = 1){
#   
#   # generate points
#   pts_unif = matrix(rnorm(n*dim), ncol = dim)
#   
#   # normalize
#   pts_unif = radius * pts_unif / sqrt(rowSums(pts_unif^2))
#   
#   return(pts_unif)
# }

# Using the mvtnorm package
library(mvtnorm)

hypersphere_uniform_points = function(n, dim, radius = 1){

  # Declare mean and variance-covariance matrix
  mu = rep(0, dim)
  var_matrix = diag(1, dim)

  pts_unif = rmvnorm(n = n, mean = mu, sigma = var_matrix)

  pts_unif = radius * pts_unif / sqrt(rowSums(pts_unif^2))

  return(pts_unif)
}


# Calculate chord lengths between all pairs of points
# Input: matrix of points. Each row is a coordinate.
chord_lengths = function(points, sorted = T){
  
  chord_lengths = as.vector(dist(points, method = "euclidean"))
  ifelse( isTRUE(sorted), return(sort(chord_lengths)), return(chord_lengths) )
}



##############################
### CDF presented in paper ###
##############################

# library(zipfR)
# 
# m_cdf = function(dist, dim, r = 1){
#   return( zipfR::Ibeta( (dist^2)/(4*r^2), dim/2, dim/2) / beta(dim/2, dim/2)  )
# }

m_cdf <- function(dist, dim, r = 1) {
  dim = dim - 1;
  pbeta( (dist^2)/(4*r^2), shape1 = dim/2, shape2 = dim/2 )  # stable regularized I_x
}



###############################
### Custom Function for CDF ###
###############################


ks_statistic_chord <- function(F_A, F_B, nA, nB, K = 100) {
  # Input checks (keep your intent to require positive integers)
  if (!is.numeric(F_A) || !is.numeric(F_B)) {
    stop("F_A and F_B must be numeric vectors.")
  }
  if (length(F_A) != length(F_B)) {
    stop("F_A and F_B must have the same length (evaluated on a common grid).")
  }
  if (length(nA) != 1 || length(nB) != 1 ||
      nA <= 0 || nB <= 0 || nA != floor(nA) || nB != floor(nB)) {
    stop("nA and nB must be positive integers.")
  }
  
  # KS statistic
  D <- max(abs(F_A - F_B), na.rm = TRUE)
  
  # Convert to double BEFORE multiplying to avoid integer overflow
  nA_d <- as.numeric(nA)
  nB_d <- as.numeric(nB)
  lambda <- D * sqrt((nA_d * nB_d) / (nA_d + nB_d))
  
  # Two-sided p-value via asymptotic series
  k <- seq_len(K)
  terms <- 2 * (-1)^(k - 1) * exp(-2 * (k^2) * (lambda^2))
  p_val <- sum(terms)
  
  # clamp to [0, 1]
  p_val <- max(0, min(1, p_val))
  
  list(statistic = D, p.value = p_val)
}
