########################################################################################################
# SURF.R
#
# Analysis of SAR images using speeded up robust features
#
# Author: Eduarda Chagas
# Date : Aug 2020
# Contact: eduarda.chagas@dcc.ufmg.br
########################################################################################################

############################################# Packages #################################################
if(!require(e1071)) install.packages("e1071")
if(!require(image.dlib)) install.packages("image.dlib")
if(!require(gtools)) install.packages("gtools")
if(!require(raster)) install.packages("raster")
source("imagematrix.R")
source("theory_information.R")

equalize <- function(data, nrow, ncol){
  
  data <- matrix(ecdf(data)(data), nrow = nrow, ncol = ncol)
  
  return(data)
}

###################################### Image Sample Parameters #######################################

#Defining the number of samples from each region analyzed
ns.guatemala = ns.canaveral.behavior1 = ns.canaveral.behavior2 = ns.munich = ns.pasture = 40
dimen.pasture = dimen.guatemala = dimen.canaveral.behavior1 = dimen.canaveral.behavior2 = dimen.munich = matrix(nrow = 40, ncol = 4)
n.total = (ns.guatemala + ns.canaveral.behavior1 + ns.canaveral.behavior2 + ns.munich + ns.pasture)

#The SAR data is available on https://drive.google.com/file/d/1jtbOcYwQfysfcUp4UhoA7lSl4_tPIqfa/view?usp=sharing and
# correspond to HHHH band of an image taken from the Cape Canaveral (acquired Sep 22, 2016)

#Pasture regions in Guatemala
row = c(1, 50, 100, 200)
col1 = seq(from = 910, to = 955, by = 5)
col23 = seq(from = 950, to = 995, by = 5)
col4 = seq(from = 1000, to = 1045, by = 5)
dimen.pasture[1:10,] = c(rep(row[1], 10), rep(128, 10), col1, rep(128, 10))
dimen.pasture[11:20,] = c(rep(row[2], 10), rep(128, 10), col23, rep(128, 10))
dimen.pasture[21:30,] = c(rep(row[3], 10), rep(128, 10), col23, rep(128, 10))
dimen.pasture[31:40,] = c(rep(row[4], 10), rep(128, 10), col4, rep(128, 10))

#Ocean regions in Cape Canaveral
row1 = c(50, 100, 150, 200, 250, 350, 450, 550, 650, 750)
row2 = c(50, 100, 150, 200, 250, 300, 350, 400, 450, 550)
row3 = c(50, 150, 250, 350, 450, 550, 650, 750, 800, 850)
row4 = c(250, 350, 450, 550, 650, 750, 850, 950, 1050)
row5 = c(50, 150, 250, 350, 450, 550, 650, 750, 850, 950, 1050)
row6 = c(50, 150, 250, 350, 450, 550, 650, 750, 800, 850, 950)
row7 = c(50, 150, 250, 350, 450, 550, 650, 750, 850, 950)
cols = c(1700, 1850, 1550, 1400, 1, 200, 350, 550)
#{Behavior 1}
dimen.canaveral.behavior1[1:9,] = c(row1[1:9], rep(128, 9), rep(cols[1], 9), rep(128, 9))
dimen.canaveral.behavior1[10,] = c(row1[10], 128, 1300, 128)
dimen.canaveral.behavior1[11:19,] = c(row2[1:9], rep(128, 9), rep(cols[2], 9), rep(128, 9))
dimen.canaveral.behavior1[20,] = c(row2[10], 128, 1350, 128)
dimen.canaveral.behavior1[21:30,] = c(row3, rep(128, 10), rep(cols[3], 10), rep(128, 10))
dimen.canaveral.behavior1[31:40,] = c(row3, rep(128, 10), rep(cols[4], 10), rep(128, 10))
#{Behavior 2}
dimen.canaveral.behavior2[1:9,] = c(row4, rep(128, 9), rep(cols[5], 9), rep(128, 9))
dimen.canaveral.behavior2[10:20,] = c(row5, rep(128, 11), rep(cols[6], 11), rep(128, 11))
dimen.canaveral.behavior2[21:30,] = c(row7, rep(128, 10), rep(cols[7], 10), rep(128, 10))
dimen.canaveral.behavior2[31:40,] = c(row7, rep(128, 10), rep(cols[8], 10), rep(128, 10))

#The SAR data is available on https://drive.google.com/file/d/1pO6p_UI9Cgdci9y6jVynAv8SrrAvv7K8/view?usp=sharing and
# correspond to HHHH band of an image taken from the Munich, Germany (acquired Jun 5, 2015) 

#Urban regions in Munich
row1 = seq(3000, 3950, by = 50)
row2 = rep(c(4300, 4350), 5)
row3 = c(rep(seq(2300, 2450, by = 50), 2), 2400, 2450)
cols = 400
cols2 = c(1300, 1300, 1350, 1350, 1400, 1400, 1450, 1450, 1500, 1500)
cols3 = c(rep(500, 4), rep(400, 4), rep(300, 2))
dimen.munich[1:20,] = c(row1, rep(128, 20), rep(cols, 20), rep(128, 20))
dimen.munich[21:30,] = c(row2, rep(128, 10), cols2, rep(128, 10))
dimen.munich[31:40,] = c(row3, rep(128, 10), cols3, rep(128, 10))

#Forest regions in Guatemala
row1 = seq(5150, 6100, by = 50)
row2 = seq(5200, 5650, by = 50)
row3 = seq(4100, 4200, by = 50)
row4 = seq(1000, 1150, by = 50)
cols = c(2700, 2800, 2930, 1930, 1870)
dimen.guatemala[1:20,] = c(row1, rep(128, 20), rep(cols[1], 20), rep(128, 20))
dimen.guatemala[21:30,] = c(row2, rep(128, 10), rep(cols[2], 10), rep(128, 10))
dimen.guatemala[31:33,] = c(row3, rep(128, 3), rep(cols[3], 3), rep(128, 3))
dimen.guatemala[34:37,] = c(row4, rep(128, 4), rep(cols[4], 4), rep(128, 4))
dimen.guatemala[38:40,] = c(row4[1:3], rep(128, 3), rep(cols[5], 3), rep(128, 3))

###################################### Function of Analysis ##########################################

SURF.analysis <- function(){
  dim.col = 40*64
  surf.data = matrix(rep(0, dim.col*200), nrow = 200, ncol = dim.col)
  #Guatemala
  sar_data = raster(paste("../../../Data/", "guatemala", "/HHHH", ".grd", sep = ""))
  for(j in c(1:ns.guatemala)){
    img = getValuesBlock(sar_data, row = dimen.guatemala[j,1], nrows = dimen.guatemala[j,2], col = dimen.guatemala[j,3], ncols = dimen.guatemala[j,4], format = "matrix")
    img.surf = array(img, dim = c(1,128,128))
    g = image_surf(img.surf)
    if(length(as.vector(g$surf)) < dim.col)
      surf.data[j,] = c(as.vector(g$surf), rep(0, dim.col-length(as.vector(g$surf))))
    else
      surf.data[j,] = as.vector(g$surf)
    cat("Guatemala ", j, "\n")
  }
  #Cape Canaveral - behavior 1
  sar_data = raster(paste("../../../Data/", "cape", "/HHHH", ".grd", sep = ""))
  for(j in c(1:ns.canaveral.behavior1)){
    img = getValuesBlock(sar_data, row = dimen.canaveral.behavior1[j,1], nrows = dimen.canaveral.behavior1[j,2], col = dimen.canaveral.behavior1[j,3], ncols = dimen.canaveral.behavior1[j,4], format = "matrix")
    img.surf = array(img, dim = c(1,128,128))
    g = image_surf(img.surf)
    if(length(as.vector(g$surf)) < dim.col)
      surf.data[ns.guatemala + j,] = c(as.vector(g$surf), rep(0, dim.col-length(as.vector(g$surf))))
    else
      surf.data[ns.guatemala + j,] = as.vector(g$surf)
    cat("Cape 1 ", j, "\n")
  }
  #Cape Canaveral - behavior 2
  sar_data = raster(paste("../../../Data/", "cape", "/HHHH", ".grd", sep = ""))
  for(j in c(1:ns.canaveral.behavior2)){
    img = getValuesBlock(sar_data, row = dimen.canaveral.behavior2[j,1], nrows = dimen.canaveral.behavior2[j,2], col = dimen.canaveral.behavior2[j,3], ncols = dimen.canaveral.behavior2[j,4], format = "matrix")
    img.surf = array(img, dim = c(1,128,128))
    g = image_surf(img.surf)
    if(length(as.vector(g$surf)) < dim.col)
      surf.data[(ns.canaveral.behavior1 + ns.guatemala) + j,] = c(as.vector(g$surf), rep(0, dim.col-length(as.vector(g$surf))))
    else
      surf.data[(ns.canaveral.behavior1 + ns.guatemala) + j,] = as.vector(g$surf)
    cat("Cape 2 ", j, "\n")
  }
  #Munich
  sar_data = raster(paste("../../../Data/", "munich", "/HHHH", ".grd", sep = ""))
  for(j in c(1:ns.munich)){
    img = getValuesBlock(sar_data, row = dimen.munich[j,1], nrows = dimen.munich[j,2], col = dimen.munich[j,3], ncols = dimen.munich[j,4], format = "matrix")
    img.surf = array(img, dim = c(1,128,128))
    g = image_surf(img.surf)
    if(length(as.vector(g$surf)) < dim.col)
      surf.data[(ns.canaveral.behavior1 + ns.canaveral.behavior2 + ns.guatemala) + j,] = c(as.vector(g$surf), rep(0, dim.col-length(as.vector(g$surf))))
    else
      surf.data[(ns.canaveral.behavior1 + ns.canaveral.behavior2 + ns.guatemala) + j,] = as.vector(g$surf)
    cat("Munich ", j, "\n")
  }
  #Pasture
  sar_data = raster(paste("../../../Data/", "guatemala", "/HHHH", ".grd", sep = ""))
  for(j in c(1:ns.pasture)){
    img = getValuesBlock(sar_data, row = dimen.pasture[j,1], nrows = dimen.pasture[j,2], col = dimen.pasture[j,3], ncols = dimen.pasture[j,4], format = "matrix")
    img.surf = array(img, dim = c(1,128,128))
    g = image_surf(img.surf)
    if(length(as.vector(g$surf)) < dim.col)
      surf.data[(ns.canaveral.behavior1 + ns.canaveral.behavior2 + ns.guatemala + ns.munich) + j,] = c(as.vector(g$surf), rep(0, dim.col-length(as.vector(g$surf))))
    else
      surf.data[(ns.canaveral.behavior1 + ns.canaveral.behavior2 + ns.guatemala + ns.munich) + j,] = as.vector(g$surf)
    cat("Pasture ", j, "\n")
  }
  write.csv(surf.data,'../Data/SURF2.csv', row.names = FALSE)
}

SURF.analysis()