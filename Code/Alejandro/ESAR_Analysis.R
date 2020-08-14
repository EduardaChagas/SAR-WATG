source("imagematrix.R")
library("png")

equalize <- function(data, nrow, ncol){
  
  data <- matrix(ecdf(data)(data), nrow = nrow, ncol = ncol)
  
  return(data)
}

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