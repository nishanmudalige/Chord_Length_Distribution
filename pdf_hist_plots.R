source("https://raw.githubusercontent.com/nishanmudalige/Chord_Length_Distribution/refs/heads/main/Code/PDF%20and%20Histograms.R")

## For reproducibility
set.seed(1)

## Set the dimension (dim) of the and number of points (n)
dim = 3
n = 1000

## Randomly generate points on the sphere
points = hypersphere_uniform_points(n, dim)
d_u = chord_lengths(points)

# Save old margins
old_par <- par(no.readonly = TRUE)

# Increase left margin only for this plot
par(mar = c(5, 4.5, 4, 2))

## PDF
hist(d_u, 
     probability = TRUE, 
     xlab = "Chord Length",
     col = "grey90",
     main = bquote(atop(Histogram~of~Simulated~Points, vs.~Theoretical~Density~over~S^.(dim-1)) ),
     cex.lab = 2.0,
     cex.axis = 2.0,
     cex.main = 2.0)

## Superimpose density on histogram
theoretical_density = chord_pdf(d_u, dim = dim)
points(d_u, theoretical_density, type = "l", lwd = 3)

## Add legend
legend("topleft",
       legend   = c("Theoretical\ndensity"),
       col      = "black",
       lty      = 1,
       lwd      = 1.5,
       cex      = 2.0,
       x.intersp = 0.5,
       seg.len  = 1.0,
       inset    = 0.0,
       bty      = "n")

par(old_par)