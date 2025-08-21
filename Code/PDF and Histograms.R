######################################################
### Functions for generating points on hypersphere ###
######################################################

# # Generate uniform points on hyper sphere
# # Based on the work by Muller (1959) and Marsaglia (1972)
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



#####################################
### Histograms and density curves ###
#####################################


# PDF of chord length distribution
chord_pdf = function(x, dim, r = 1){
  
  n = dim-1 # dimension
  
  return( (x / (r^2 * beta(n/2, 1/2) )) * ( (x^2/r^2) - (x^4/(4*r^4)) )^((n-2)/2) )
}