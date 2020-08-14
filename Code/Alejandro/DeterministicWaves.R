require(plotly)
source("imagematrix.R")
library("png")

equalize <- function(data, nrow, ncol){
  
  data <- matrix(ecdf(data)(data), nrow = nrow, ncol = ncol)
  
  return(data)
}

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

plot_ly(z = z, type = "surface")

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
plot(imagematrix(equalize(z1Look, 128, 128)))
z3Looks <- z * Speckle3Looks
plot(imagematrix(equalize(z3Looks, 128, 128)))
z8Looks <- z * Speckle8Looks
plot(imagematrix(equalize(z8Looks, 128, 128)))
z100Looks <- z * Speckle100Looks
plot(imagematrix(equalize(z100Looks, 128, 128)))


### Three complex bands from the ESAR sensor
load("ESAR.Rdata")

typeof(SingleLookComplex)

HV_Int <- imagematrix(equalize(matrix(Mod(SingleLookComplex[[2]]), nrow = 7134), nrow = 7134, ncol = 1475))
HV_Int <- t(HV_Int)
HH_Int <- imagematrix(equalize(matrix(Mod(SingleLookComplex[[1]]), nrow = 7134), nrow = 7134, ncol = 1475))
HH_Int <- t(HH_Int)
VV_Int <- imagematrix(equalize(matrix(Mod(SingleLookComplex[[3]]), nrow = 7134), nrow = 7134, ncol = 1475))
VV_Int <- t(VV_Int)
dim(HV_Int)


X11(width = 7134/5, height = 1475/5)
plot(HV_Int)

X11(width = 7134/5, height = 1475/5)
### Lexicographic color assignation
plot(imagematrix(c(HH_Int, HV_Int, VV_Int), 
                 type = "rgb", ncol = 1475, nrow = 7134))
writePNG(imagematrix(c(HH_Int, HV_Int, VV_Int), 
                     type = "rgb", ncol = 1475, nrow = 7134), target = "A_image_Test1.png")

X11(width = 7134/5, height = 1475/5)
### Pauli decomposition color assignation
plot(imagematrix(
  c(
    equalize(abs(HH_Int-VV_Int)), 
    HV_Int, 
    equalize(HH_Int+VV_Int)), 
  type = "rgb", ncol = 1475, nrow = 7134)
  )


writePNG(imagematrix(
  c(
    equalize(abs(HH_Int-VV_Int), nrow = 7134, ncol = 1475), 
    HV_Int, 
    equalize(HH_Int+VV_Int, nrow = 7134, ncol = 1475)), 
  type = "rgb", ncol = 1475, nrow = 7134), target = "A_image_Test2.png")

######## Get image
intensity_matrix = imagematrix(c(HH_Int, HV_Int, VV_Int), type = "rgb", ncol = 1475, nrow = 7134)
equalized_matrix <- equalize(intensity_matrix, ncol = 1475, nrow = 7134)

col.element = 7000
row.element = 900
writePNG(intensity_matrix[row.element:(row.element + 128), col.element:(col.element + 128), ], target = "pasto.png")

row <- c(rep(650, 8), rep(490, 7), rep(515, 5), rep(840, 3), 850, 450, 450, 440, 
         470, 460, 450, 480, rep(1200, 6), 700, 690, 900)

col.pasture <- c(1550, 1678, 1700, 1560, 1575, 1590, 1610, 1650, 2000, 1990, 2010, 
                 2050, 2020, 2075, 2060, 1990, 2000, 2010, 2020, 2050, 4450, 4500, 
                 4430, 4410, rep(4300, 5), 4290, 5950, 5960, 5940, 5900, 5800, 5850,
                 6500, rep(7000, 3))