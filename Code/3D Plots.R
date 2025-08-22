setwd("~/Desktop")

# ---- Full, self-contained R code using rgl (10 points + all chords) ----
# install.packages("rgl")  # uncomment if needed
library(rgl)

# Generate n points uniformly on the surface of a (dim-1)-sphere of given radius
hypersphere_uniform_points = function(n, dim, radius = 1){
  # generate points
  pts_unif = matrix(rnorm(n*dim), ncol = dim)
  
  # normalize
  pts_unif = pts_unif / sqrt(rowSums(pts_unif^2))
  
  return(pts_unif*radius)
}

# Parameters
# set.seed(2025)
n_points <- 5
radius   <- 1

# Sample points on the unit sphere in 3D
points <- hypersphere_uniform_points(n_points, 3, radius)

# Open interactive 3D device
# par3d(windowRect = c(20, 30, 3840, 2160))
open3d()
bg3d("white")
aspect3d(1, 1, 1)



# For saving image to file
# Define desired dimensions and resolution
dpi_val <- 5000
width_inches <- 11
height_inches <- 8.5

# Calculate pixel dimensions
width_pixels <- width_inches * dpi_val
height_pixels <- height_inches * dpi_val



# --- Smooth, transparent sphere as a parametric surface ---
n_theta <- 80
n_phi   <- 160
theta <- seq(0, pi,    length.out = n_theta)
phi   <- seq(0, 2*pi,  length.out = n_phi)
x <- outer(sin(theta), cos(phi)) * radius
y <- outer(sin(theta), sin(phi)) * radius
z <- outer(cos(theta), rep(1, length(phi))) * radius

surface3d(
  x, y, z,
  color     = "lightblue",
  alpha     = 0.12,
  front     = "fill",
  back      = "fill",
  lit       = TRUE,
  specular  = "gray50",
  shininess = 50
)

# --- Plot the sampled points (in red) ---
points3d(points, col = "red", size = 7)

# --- All chords: vectorized segments3d (no loop) ---
idx <- t(utils::combn(nrow(points), 2))  # each row is a pair (i, j)

x0 <- points[idx[, 1], 1]; y0 <- points[idx[, 1], 2]; z0 <- points[idx[, 1], 3]
x1 <- points[idx[, 2], 1]; y1 <- points[idx[, 2], 2]; z1 <- points[idx[, 2], 3]

# Interleave start/end rows: (x0,y0,z0), (x1,y1,z1), (x0,y0,z0), (x1,y1,z1), ...
xyz <- cbind(
  c(rbind(x0, x1)),
  c(rbind(y0, y1)),
  c(rbind(z0, z1))
)

segments3d(
  xyz,
  color = "black",
  alpha = 0.3,
  lwd   = 2
)

# Axes and labels
axes3d(edges = c("x--", "y--", "z"))
title3d(xlab = "X", ylab = "Y", zlab = "Z")




# # Save the rgl plot as a PNG with 1000 DPI
# png(filename = "rgl_plot_1000dpi.png", 
#     width = width_pixels, 
#     height = height_pixels, 
#     res = dpi_val) # 'res' argument sets the DPI
# 
# 
# snapshot3d("your_plot_name.png")
# dev.off()
# 
# # Close the rgl window (optional)
# close3d()