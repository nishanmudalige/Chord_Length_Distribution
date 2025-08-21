
## Sample Code to Obtain Histogram and Density Curve of Chord Length Distribution

```
source("https://raw.githubusercontent.com/nishanmudalige/Chord_Length_Distribution/refs/heads/main/Code/PDF%20and%20Histograms.R")

## For reproducibility
set.seed(1)

## Set the dimension (dim) and number of points (n)
dim = 6
n = 1000

## Randomly generate points on the sphere
points = hypersphere_uniform_points(n, dim)
d_u = chord_lengths(points)

## PDF
hist(d_u, 
     probability = TRUE, 
     xlab = "Chord Lengths",
     col = "grey90",
     main = bquote(atop(Histogram~of~Simulated~Points, vs.~Theoretical~Density~over~S^.(dim-1)) ))

## Superimpose density on histogram
theoretical_density = chord_pdf(d_u, dim = dim)
points(d_u, theoretical_density, type = "l", lwd = 2)

## Add legend
legend("topleft",
       legend = c("Theoretical\ndensity"),
       col    = c("black"),
       lty    = c(1),
       lwd    = c(2),
       inset  = 0.02,
       bty    = "n") 
```
