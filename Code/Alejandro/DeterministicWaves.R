require(plotly)

hilbertcurve = unlist(read.table("../Data/Hilbert/HilbertCurves.txt")) + 1

### Example of deterministic texture with speckle
phantom <- matrix(rep(0, 128*128), nrow = 128)
x <- y <- seq(-2*pi, 2*pi, length.out = 128)

deterministic <- function(x, y) {sin(4*x+.5*y)}

z <- outer(x,y,deterministic)

plot(
  imagematrix(
    equalize(z, 128, 128)
  )
)

#plot_ly(z = z, type = "surface")

### Adding speckle to our image
range(z) # it is in [-1, 1], let's scale it to [0,1]
z <- normalize(z)
range(z)

set.seed(1234567890) # for reproducibility
Speckle1Look <- matrix(rexp(128*128), nrow = 128)
Speckle3Looks <- matrix(rgamma(128*128, shape=3, rate=3), nrow = 128)
Speckle8Looks <- matrix(rgamma(128*128, shape=8, rate=8), nrow = 128)
Speckle100Looks<- matrix(rgamma(128*128, shape=100, rate=100), nrow = 128)

z1Look <- z * Speckle1Look
z3Looks <- z * Speckle3Looks
z8Looks <- z * Speckle8Looks
z100Looks <- z * Speckle100Looks

plot(imagematrix(equalize(z1Look, 128, 128)))
plot(imagematrix(equalize(z3Looks, 128, 128)))
plot(imagematrix(equalize(z8Looks, 128, 128)))
plot(imagematrix(equalize(z100Looks, 128, 128)))
