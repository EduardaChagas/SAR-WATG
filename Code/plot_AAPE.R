########################################################################################################
# plot_AAPE.R
#
# Generate Plot of Analysis of SAR images using Hilbert space-filling curves and AAPE
#
# Author: Eduarda Chagas
# Date : May 9, 2020
# Contact: eduarda.chagas@dcc.ufmg.br
########################################################################################################

############################################# Packages #################################################

if(!require(gtools)) install.packages("gtools")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(ggthemes)) install.packages("ggthemes")
if(!require(ggpubr)) install.packages("ggpubr")
source("theory_information.R")

###################################### Function of Plot ################################################

HC.Plane.no.cota <- function(dimension, color.signal, shape.signal, signal.values){
  
  shape.select = c(17,18,19,8)
  XMIN = min(signal.values[,1]) + 0.00005
  XMAX = min(max(signal.values[,1]) + 0.00005, 1)
  YMIN = max(0,min(signal.values[,2]) - 0.00005)
  YMAX = max(signal.values[,2]) + 0.00005
  
  # Paleta montada a partir de https://coolors.co/
  rainbow.colors = palette(c("#3F84E5",
                             "#B20D30", 
                             "#3F784C",
                             "#EFCD4A"))
  
  Color = rainbow.colors[color.signal]
  Shape = shape.select[shape.signal]
  Regions =  c("Forest", "Ocean", "", "Urban")[color.signal]
  signal.values = data.frame("H" = signal.values[,1], "C" = signal.values[,2], "Color" = Color, "Shape" = Shape, "Regions" = Regions)
  
  p = ggplot(signal.values, aes(H, C, color = Regions, shape = Shape)) + geom_point(size = 2) +
    scale_shape_identity() +
    xlim(limits=c(XMIN, XMAX)) + ylim(limits=c(YMIN, YMAX)) + 
    theme_few(base_size = 18, base_family = "serif")  + 
    theme(plot.title = element_text(hjust=0.5)) + 
    scale_colour_few("Dark")
  print(p)
  return(p)
}

###################################### Function of Analysis ##########################################

plot.AAPE.d3t1 <- function(file.name){
  
  n = 3 #Dimension parameter
  tal = 1 #Delay parameter
  hilbertcurve = unlist(read.table("../Data/Hilbert/HilbertCurves128.txt")) + 1
  types = c(rep(1,40), rep(2,80), rep(4, 40))
  regions = c(rep(1,40), rep(2,80), rep(4, 40))
  n.total = 160
  
  Entropy.Complexity.csv = read.csv(file = file.name, header=TRUE, sep=",")
  Entropy.Complexity = matrix(nrow = n.total, ncol = 2)
  
  Entropy.Complexity[,1] = Entropy.Complexity.csv[1:160, 1]
  Entropy.Complexity[,2] = Entropy.Complexity.csv[1:160, 2]
  
  plot.AAPE = HC.Plane.no.cota(n, regions, types, Entropy.Complexity) + ggtitle(expression(italic("AAPE (A = 1)"))) +
    labs(x="", y="")
  return(plot.AAPE)
  
}

pdf("AAPEA05.pdf", width = 10, height = 8) 
plot.AAPE.A05 = plot.AAPE.d3t1("../Data/EntropyComplexityAAPED3T1A05.csv")
dev.off() 
