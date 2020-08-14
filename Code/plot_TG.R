########################################################################################################
# plot_TG.R
#
# Generate Plot of Analysis of SAR images using TG
#
# Author: Eduarda Chagas
# Date : Mar 18, 2020
# Contact: eduarda.chagas@dcc.ufmg.br
########################################################################################################

############################################# Packages #################################################

if(!require(gtools)) install.packages("gtools")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(ggthemes)) install.packages("ggthemes")
if(!require(ggpubr)) install.packages("ggpubr")
if(!require(latex2exp)) install.packages("latex2exp")
source("theory_information.R")

###################################### Function of Plot ################################################

HC.Plane.no.cota <- function(dimension, color.signal, shape.signal, signal.values){
  
  shape.select = c(17,18,19,8)
  XMIN = min(signal.values[,1]) + 0.0005
  XMAX = min(max(signal.values[,1]) + 0.0005, 1)
  YMIN = max(0,min(signal.values[,2]) - 0.008)
  YMAX = max(signal.values[,2]) + 0.008
  
  # Paleta montada a partir de https://coolors.co/
  rainbow.colors = palette(c("#3F84E5",
                             "#B20D30", 
                             "#3F784C",
                             "#EFCD4A"))
  
  Color = rainbow.colors[color.signal]
  Shape = shape.select[shape.signal]
  Regions =  c("Forest", "Ocean", "Urban", "Pasture")[color.signal]
  signal.values = data.frame("H" = signal.values[,1], "C" = signal.values[,2], "Color" = Color, "Shape" = Shape, "Regions" = Regions)
  
  p = cotas(factorial(dimension)^2)
  p = p + 
    geom_point(data = signal.values, aes(x = H, y = C, color = Regions, shape = Shape), size = 2) +
    labs(x = TeX("\\textit{H}"), y = TeX("\\textit{C}"))  +
    xlim(limits=c(XMIN, XMAX)) + ylim(limits=c(YMIN, YMAX)) + 
    scale_shape_identity() +
    theme_few(base_size = 18, base_family = "serif")  + 
    theme(plot.title = element_text(hjust=0.5)) + 
    scale_colour_few("Dark")
  print(p)
  return(p)
}


###################################### Function of Analysis ##########################################

plot.TG.analysis <- function(){
  
  n = 3 #Dimension parameter
  tal = 1 #Delay parameter
  plots = array(list(), 20)
  hilbertcurve = unlist(read.table("../Data/Hilbert/HilbertCurves128.txt")) + 1
  types = c(rep(1,40), rep(2,80), rep(3, 40), rep(4, 40))
  regions = c(rep(1,40), rep(2,80), rep(3, 40), rep(4, 40))
  n.total = 200
  
  Entropy.Complexity.csv = read.csv(file="../Data/EntropyComplexityTGD3T1.csv", header=TRUE, sep=",")
  Entropy.Complexity = matrix(nrow = n.total, ncol = 2)
  
  Entropy.Complexity[,1] = Entropy.Complexity.csv[, 1]
  Entropy.Complexity[,2] = Entropy.Complexity.csv[, 2]
  
  plot.TG = HC.Plane.no.cota(n, regions, types, Entropy.Complexity) + ggtitle(expression(italic("Transition graph")))
  return(plot.TG)
}

#pdf("TG.pdf", width = 10, height = 8) 
plot.TG = plot.TG.analysis()
#dev.off() 