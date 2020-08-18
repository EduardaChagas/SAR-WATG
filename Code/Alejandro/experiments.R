########################################################################################################
# DeterministicWavesAnalysis.R
#
#
# Author: Eduarda Chagas
# Date : Aug 2020
# Contact: eduarda.chagas@dcc.ufmg.br
########################################################################################################

############################################# Packages #################################################
if(!require(gtools)) install.packages("gtools")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(ggrepel)) install.packages("ggrepel")
if(!require(ggthemes)) install.packages("ggthemes")
if(!require(latex2exp)) install.packages("latex2exp")
source("imagematrix.R")
source("theory_information.R")

equalize <- function(data, nrow, ncol){
  
  data <- matrix(ecdf(data)(data), nrow = nrow, ncol = ncol)
  
  return(data)
}

############################### Transition graphs functions ############################################

FP <- function(n, dimension, delay){
  dyn.load("FormationPatterns.so")
  p <- .Call("FormationPatterns", n, dimension, delay)
  p = t(p) + 1
  return(p)
}

formationPattern <- function(serie, dimension, delay, option){
  i = 1
  n = length(serie)
  p_patterns = elements = index2 = matrix(nrow=n,ncol=dimension)
  index = c(0:(dimension-1))
  
  index2 = FP(length(serie), dimension, delay)
  
  while((i + ((dimension-1)*delay)) <= n){ 
    elements[i,] = serie[index2[i,]]
    p_patterns[i,] = index[order(elements[i,])]
    i = i + 1
  }
  
  if(option == 0){
    p_patterns = na.omit(p_patterns)
    return(p_patterns[1:dim(p_patterns)[1],])
  }else if(option == 1){
    elements = na.omit(elements)
    return(elements[1:dim(elements)[1],])    
  }else{
    index2 = na.omit(index2)
    return(index2[1:dim(index2)[1],])    
  }
}

define.symbols <- function(dimension){
  d = c(1:dimension)
  symbol = matrix(unlist(permutations(n=dimension, r = dimension, v = d)),nrow = factorial(dimension),ncol = dimension,byrow = FALSE)
  symbol
}

pattern.wedding <- function(patterns){
  patterns = patterns + 1
  m = dim(patterns)[1]
  D = dim(patterns)[2]
  symbols = define.symbols(D)
  wedding = rep(0, m)
  for(i in 1:m){
    e = 0
    j = 1
    stop = F
    while(j <= factorial(D) && stop == F){
      if(sum(symbols[j,] == patterns[i,]) == D){
        wedding[i] = j
        stop = T
      }
      j = j + 1
    }
  }
  wedding
}

transition.graph.weight <- function(series, dimension, delay){
  
  graph = matrix(0, nrow = factorial(dimension), ncol = factorial(dimension))
  patterns = formationPattern(series, dimension, delay, 0)
  elements = formationPattern(series, dimension, delay, 1)
  wedding = pattern.wedding(patterns)
  m = length(wedding)
  weight.total = 0
  
  for(i in 1:(m-1)){
    weight.i1 = (max(elements[i,]) - min(elements[i,]))
    weight.i2 = (max(elements[i+1,]) - min(elements[i+1,]))
    graph[wedding[i],wedding[i+1]] = graph[wedding[i],wedding[i+1]] + abs(weight.i1 - weight.i2)
    weight.total = weight.total + abs(weight.i1 - weight.i2)
  }
  graph = graph/weight.total
  return(graph)
}

############################################## Analysis ################################################

hilbertcurve = unlist(read.table("../Data/Hilbert/HilbertCurves.txt")) + 1

set.seed(1234567890) # for reproducibility
Speckle1Look <- matrix(rexp(128*128), nrow = 128)
set.seed(1234567890) # for reproducibility
Speckle3Looks <- matrix(rgamma(128*128, shape=3, rate=3), nrow = 128)
set.seed(1234567890) # for reproducibility
Speckle8Looks <- matrix(rgamma(128*128, shape=8, rate=8), nrow = 128)
set.seed(1234567890) # for reproducibility
Speckle100Looks<- matrix(rgamma(128*128, shape=100, rate=100), nrow = 128)
set.seed(1234567890) # for reproducibility
Speckle500Looks<- matrix(rgamma(128*128, shape=500, rate=500), nrow = 128)

x <- y <- seq(-2*pi, 2*pi, length.out = 128)
deterministic <- function(x, y) {sin(4*x+.5*y)}
z <- outer(x,y,deterministic)

### Adding speckle to our image
range(z) # it is in [-1, 1], let's scale it to [0,1]
z <- normalize(z)
range(z)

waves.types = c("z", "z1Look", "z3Looks", "z8Looks", "z100Looks", "z500Looks")
Entropy.Complexity = data.frame(H = length(6), C = length(6), names = as.factor(waves.types))
dimension = 6
ts = z[hilbertcurve]/max(z[hilbertcurve])
g = transition.graph.weight(ts, dimension, 1)
Entropy.Complexity[1,1] = shannonNormalized(as.vector(g))
Entropy.Complexity[1,2] = Ccomplexity(as.vector(g))

z1Look <- z * t(Speckle1Look)
ts = z1Look[hilbertcurve]/max(z1Look[hilbertcurve])
g = transition.graph.weight(ts, dimension, 1)
Entropy.Complexity[2,1] = shannonNormalized(as.vector(g))
Entropy.Complexity[2,2] = Ccomplexity(as.vector(g))

z3Looks <- z * t(Speckle3Looks)
ts = z3Looks[hilbertcurve]/max(z3Looks[hilbertcurve])
g = transition.graph.weight(ts, dimension, 1)
Entropy.Complexity[3, 1] = shannonNormalized(as.vector(g))
Entropy.Complexity[3, 2] = Ccomplexity(as.vector(g))

z8Looks <- z * t(Speckle8Looks)
ts = z8Looks[hilbertcurve]/max(z8Looks[hilbertcurve])
g = transition.graph.weight(ts, dimension, 1)
Entropy.Complexity[4,1] = shannonNormalized(as.vector(g))
Entropy.Complexity[4, 2] = Ccomplexity(as.vector(g))

z100Looks <- z * t(Speckle100Looks)
ts = z100Looks[hilbertcurve]/max(z100Looks[hilbertcurve])
g = transition.graph.weight(ts, dimension, 1)
Entropy.Complexity[5, 1] = shannonNormalized(as.vector(g))
Entropy.Complexity[5, 2] = Ccomplexity(as.vector(g))

z500Looks <- z * t(Speckle500Looks)
ts = z500Looks[hilbertcurve]/max(z500Looks[hilbertcurve])
g = transition.graph.weight(ts, dimension, 1)
Entropy.Complexity[6, 1] = shannonNormalized(as.vector(g))
Entropy.Complexity[6, 2] = Ccomplexity(as.vector(g))

##This figure illustrates the effect of the number of looks on 
#the (h,c) point. Speckle and signal are comparable, since the signal is in [0,1], 
#and the expected value of the speckle is 1. D = 6, looks = c(z, 500, 100, 1, 8, 3)
p = cotas(factorial(dimension)^2)
p = p + geom_point(data = Entropy.Complexity, aes(x = H, y = C),size = 1.5, alpha = .4) + 
  geom_label_repel(data = Entropy.Complexity, aes(x = H, y = C, label = names), 
                   segment.size = 0.5, min.segment.length = 0, force = 18, alpha = .4) +
  ggtitle(expression(italic("H x C Plane"))) + 
  xlim(limits=c(0.3, 0.6)) + ylim(limits=c(0.3, 0.6)) +  
  theme_few() 
p

