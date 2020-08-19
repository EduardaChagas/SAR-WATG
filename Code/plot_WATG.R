########################################################################################################
# plot_WATG.R
#
# Generate Plot of Analysis of SAR images using Hilbert space-filling curves and WATG
#
# Author: Eduarda Chagas
# Date : Mar 2020
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

HC.Plane.zoom <- function(dimension, color.signal, shape.signal, signal.values, horizontal, vertical, d, t){
  
  shape.select <- c(17,18,19,8)
  XMIN = min(signal.values[,1]) + 0.00005
  XMAX = min(max(signal.values[,1]) + 0.0005, 1)
  YMIN = max(0,min(signal.values[,2]) - 0.005)
  YMAX = max(signal.values[,2]) + 0.005
  
  # Paleta montada a partir de https://coolors.co/
  rainbow.colors = palette(c("#3F84E5",
                             "#B20D30", 
                             "#3F784C",
                             "#EFCD4A"))
  
  Color = rainbow.colors[color.signal]
  Shape = shape.select[shape.signal]
  Regions =  c("Forest", "Ocean", "Urban", "Pasture")[color.signal]
  signal.values = data.frame("H" = signal.values[,1], "C" = signal.values[,2], "Color" = Color, "Shape" = Shape, "Regions" = Regions)
  
  p = cotas(dimension)
  p = p + 
    geom_point(aes(x = H, y = C, color = Regions), alpha = I(0.25), signal.values, shape = Shape, size = 2) +
    xlim(limits=c(XMIN, XMAX)) + ylim(limits=c(YMIN, YMAX)) +  
    theme_few(base_size = 18, base_family = "serif")  + 
    theme(plot.title = element_text(hjust=0.5)) + 
    scale_colour_few("Dark")
  
  xlab = ylab = ""
  if(horizontal == 1 )
    ylab = paste("D = ", d)
  if(vertical){
    xlab = bquote(tau==.(t))
  }
  
  p = p + labs(x = xlab, y = ylab, parse = TRUE)
  return(p)
}

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
    scale_shape_identity() +
    xlim(limits=c(XMIN, XMAX)) + ylim(limits=c(YMIN, YMAX)) + 
    theme_few(base_size = 18, base_family = "serif")  + 
    theme(plot.title = element_text(hjust=0.5)) + 
    scale_colour_few("Dark")
  print(p)
  return(p)
}

HC.Plane.cota <- function(dimension, color.signal, shape.signal, signal.values){
  
  shape.select = c(17,18,19,8)
  XMIN = 0
  XMAX = 1
  YMIN = 0
  YMAX = 0.4
  
  # Paleta montada a partir de https://coolors.co/
  rainbow.colors = palette(c("#3F84E5",
                             "#B20D30", 
                             "#3F784C",
                             "#EFCD4A"))
  
  Color = rainbow.colors[color.signal]
  Shape = shape.select[shape.signal]
  Texture = color.signal
  Regions =  c("Forest", "Ocean", "Urban", "Pasture")[color.signal]
  signal.values <- data.frame("H" = signal.values[,1], "C" = signal.values[,2], "Color" = Color, "Shape" = Shape, "Regions" = Regions)
  p = cotas(factorial(dimension)^2)
  p = p + 
    geom_point(aes(H, C, color = Regions, shape = Shape), size = 3) +
    labs(x="", y="") + xlim(limits=c(XMIN, XMAX)) + ylim(limits=c(YMIN, YMAX)) + 
    theme_few(base_size = 18, base_family = "serif")  + theme(plot.title = element_text(hjust=0.5)) + 
    scale_colour_few("Dark")
  return(p)
}

###################################### Function of Analysis ##########################################

plot.transition.graph.analysis <- function(){
  
  n = c(3,4,5,6) #Dimension parameter
  tal = c(1,2,3,4,5) #Delay parameter
  plots = array(list(), 20)
  hilbertcurve = unlist(read.table("../Data/Hilbert/HilbertCurves128.txt")) + 1
  types = c(rep(1,40), rep(2,80), rep(3, 40), rep(4, 40))
  regions = c(rep(1,40), rep(2,80), rep(3, 40), rep(4, 40))
  n.total = 200
  a = 1
  
  for(d in n){
    for(t in tal){
      Entropy.Complexity.csv = read.csv(paste('../Data/EntropyComplexityWATGD', d, 'T', t, '.csv', sep = ""))
      
      horizontal = vertical = 0
      if(t == 1 && (d == 3 || d == 4 || d == 5)){
        horizontal = 1
      }else if(t == 1 && d == 6){
        horizontal = vertical = 1
      }else if(d == 6 && (t == 2 || t == 3 || t == 4 || t == 5)){
        vertical = 1
      }else{
        horizontal = vertical = 0
      }
      
      cat("- Plane: ", a, "de 20 ", "\n")
      Entropy.Complexity = matrix(nrow = n.total, ncol = 2)
      
      Entropy.Complexity[,1] = Entropy.Complexity.csv[, 1]
      Entropy.Complexity[,2] = Entropy.Complexity.csv[, 2]
      plots[[a]] = HC.Plane.zoom(factorial(d)^2, regions, types, Entropy.Complexity, horizontal, vertical, d, t)
      a = a + 1
    }
  }
  
  p = ggarrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]],
                plots[[6]], plots[[7]], plots[[8]], plots[[9]], plots[[10]],
                plots[[11]], plots[[12]], plots[[13]], plots[[14]], plots[[15]],
                plots[[16]], plots[[17]], plots[[18]], plots[[19]], plots[[20]],
                ncol=5, nrow=4, common.legend = TRUE, legend = "right") + 
    ggtitle(TeX("\\textit{H} $ \\times $ \\textit{C Plane}")) +
    xlab(expression(italic(H))) + ylab(expression(italic(C))) + 
    labs(colour=expression(italic(Regions))) +
    theme_clean() + theme(text=element_text(size=14, family="Times"), axis.text.x=element_blank(), axis.text.y=element_blank(),plot.title = element_text(hjust=0.5)) + 
    guides(colour = guide_legend(override.aes = list(size=3)))
  return(p)
}

plot.d3t1 <- function(){
  
  n = 3 #Dimension parameter
  tal = 1 #Delay parameter
  hilbertcurve = unlist(read.table("../Data/Hilbert/HilbertCurves128.txt")) + 1
  types = c(rep(1,40), rep(2,80), rep(3, 40), rep(4, 40))
  regions = c(rep(1,40), rep(2,80), rep(3, 40), rep(4, 40))
  n.total = 200
  
  Entropy.Complexity = read.csv(file="../Data/EntropyComplexityWATGD3T1.csv", header=TRUE, sep=",")
    
  plot.WATG = HC.Plane.no.cota(n, regions, types, Entropy.Complexity) + ggtitle(expression(italic("WATG"))) 
  return(plot.WATG)
  
}

pdf("WATG.pdf", width = 10, height = 8) 
plot.WATG = plot.d3t1()
dev.off() 

#p = plot.transition.graph.analysis()
#pdf("WATGHC.pdf", width = 24, height = 15)
#p
#dev.off()
