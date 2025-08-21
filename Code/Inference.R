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



###############
### Example ###
###############


# For reproducibility
set.seed(1)

# First run Uniform Point Picking Code

dim = 50
n = 100
r = 1

points = hypersphere_uniform_points(n, dim)
d_u = chord_lengths(points)

# ECDF
ecdf_u = ecdf(d_u)
# plot( ecdf_u )


# Isolate CDF values of ECDF

env = environment(ecdf_u)
ecdf_y = env$y



# Using CDF in Paper

m_cdf_points = m_cdf(d_u, dim = dim, r = r)

ks_statistic_chord(ecdf_y, m_cdf_points, 
                              nA = length(ecdf_y), 
                              nB = length(m_cdf_points)) 


# plots
# CDF plot
# plot(d_u, m_cdf_points, type = "l")

# Compare with ECDF

# Plot theoretical CDF
plot(d_u, m_cdf_points, type = "l", col="red", 
     ylab = "Comulative CDF",
     xlab = "Chord length",
     xlim = c(0,2),
     # main = "ECDF and Chord Length CDF (N = 7, n = 100, r = 1)")
     main = bquote(ECDF~and~Theoretical~Chord~Length~CDF~over~S^.(dim)~'('~n~'='~.(n)~','~r~'='~.(r)~')' ))


# Superimpose ECDF
points(d_u, ecdf_y, col = "blue", lwd = 2, type = 'l', lty=3)


# Add legend
legend("topleft", 
       legend = c("Chord Length", "ECDF"), 
       col = c("red", "blue"), lty = c(1, 2), 
       # pch = c(19, 18), 
       cex = 0.8,
       lwd    = c(2, 2),
       inset  = 0.02,    # 2% inset from the margins
       bty    = "n")



