source("https://raw.githubusercontent.com/nishanmudalige/Chord_Length_Distribution/refs/heads/main/Code/Inference.R")

## For reproducibility
set.seed(1)

## Set the dimension (dim) and number of points (n)
dim = 3
n = 10
r = 1

## Randomly generate points on the sphere
points = hypersphere_uniform_points(n, dim)
d_u = sort(chord_lengths(points))
d_u_extend = c(d_u, 
        seq(-1, 0.5, by = 0.01),
        seq(2, 2.5, by = 0.01))
d_u_extend = sort(d_u_extend)

# Emperical CDF
ecdf_u = ecdf(d_u)

## Optional plot ECDF
## plot(ecdf_u)

## Isolate CDF values of ECDF
env = environment(ecdf_u)
ecdf_y = env$y

## Get theoretical values from CDF in paper within support for simulated points
m_cdf_points = m_cdf(d_u, dim = dim)

## Get theoretical values from CDF in paper for larger domain
m_cdf_points_extend = m_cdf(d_u_extend, dim = dim)


## Calculate Kolmogorov Smirnov Statistic
ks_statistic_chord(ecdf_y, m_cdf_points, 
                   nA = length(ecdf_y), 
                   nB = length(m_cdf_points))


## Plot theoretical CDF
plot(d_u_extend, m_cdf_points_extend, type = "l", col="red", 
     ylab = "Comulative CDF",
     xlab = "Chord length",
     xlim = c(0,2),
     ylim = c(0,1),
     lwd  = 5,
     main = bquote(ECDF~and~Theoretical~Chord~Length~CDF~over~S^.(dim-1)~'('~n~'='~.(n)~','~r~'='~.(r)~')' ),
     cex.lab = 2.0,
     cex.axis = 2.0,
     cex.main = 2.0)


## Superimpose ECDF
points(d_u, ecdf_y, col = "blue", lwd = 5, type = 'l', pch = 19)


## Add legend
legend("topleft",
       legend = c("Chord Length", "ECDF"),
       col = c("red", "blue"), lty = c(1, 3),
       # pch = c(19, 18),
       cex = 1.75,
       lwd    = c(5, 5),
       inset  = -0.02,
       bty    = "n",
       x.intersp = 0.5,
       seg.len  = 1.0,
       y.intersp = 0.34)
