
![GitHub License](https://img.shields.io/github/license/nishanmudalige/Chord_Length_Distribution)
![GitHub top language](https://img.shields.io/github/languages/top/nishanmudalige/Chord_Length_Distribution)


---

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

---

## Sample Code for Inference

```
source("https://raw.githubusercontent.com/nishanmudalige/Chord_Length_Distribution/refs/heads/main/Code/Inference.R")

## For reproducibility
set.seed(1)

## Set the dimension (dim) and number of points (n)
dim = 5
n = 100

## Randomly generate points on the sphere
points = hypersphere_uniform_points(n, dim)
d_u = chord_lengths(points)

# Emperical CDF
ecdf_u = ecdf(d_u)

## Optional plot ECDF
## plot(ecdf_u)

## Isolate CDF values of ECDF
env = environment(ecdf_u)
ecdf_y = env$y

## Get theoretical values from CDF in paper
m_cdf_points = m_cdf(d_u, dim = dim)

## Calculate Kolmogorov Smirnov Statistic
ks_statistic_chord(ecdf_y, m_cdf_points, 
                   nA = length(ecdf_y), 
                   nB = length(m_cdf_points)) 

## Plot theoretical CDF
plot(d_u, m_cdf_points, type = "l", col="red", 
     ylab = "Comulative CDF",
     xlab = "Chord length",
     xlim = c(0,2),
     main = bquote(ECDF~and~Theoretical~Chord~Length~CDF~over~S^.(dim)~'('~n~'='~.(n)~','~r~'='~.(r)~')' ))


## Superimpose ECDF
points(d_u, ecdf_y, col = "blue", lwd = 2, type = 'l', lty=3)


## Add legend
legend("topleft", 
       legend = c("Chord Length", "ECDF"), 
       col = c("red", "blue"), lty = c(1, 2), 
       # pch = c(19, 18), 
       cex = 0.8,
       lwd    = c(2, 2),
       inset  = 0.02,
       bty    = "n")
```
