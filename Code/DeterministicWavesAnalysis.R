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
if(!require(scales)) install.packages("scales")
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

x <- y <- seq(-2*pi, 2*pi, length.out = 128)
deterministic <- function(x, y) {sin(4*x+.5*y)}
z <- outer(x,y,deterministic)
range(z) # it is in [-1, 1], let's scale it to [0,1]
z <- normalize(z)

plot(
  imagematrix(
    equalize(z, 128, 128)
  )
)

Entropy.Complexity = data.frame(H = length(12), C = length(12), "Speckle Looks" =length(12))

ts = z[hilbertcurve]/max(z[hilbertcurve])
g = transition.graph.weight(ts, 6, 1)
Entropy.Complexity[1,1] = shannonNormalized(as.vector(g))
Entropy.Complexity[1,2] = Ccomplexity(as.vector(g))

a = 2
waves.types = 0
looks = c(1, seq(from = 50, to = 500, by = 50))
for(l in looks){
  set.seed(1234567890) # for reproducibility
  SpeckleLooks <- matrix(rgamma(128*128, shape = l, rate = l), nrow = 128)
  waves.types = c(waves.types, l)
  zLook <- z * SpeckleLooks 
  ts = zLook[hilbertcurve]/max(zLook[hilbertcurve])
  g = transition.graph.weight(ts, 6, 1)
  Entropy.Complexity[a, 1] = shannonNormalized(as.vector(g))
  Entropy.Complexity[a, 2] = Ccomplexity(as.vector(g))
  a = a + 1
}
waves.types = as.factor(waves.types)
Entropy.Complexity$Speckle.Looks = waves.types

##This figure illustrates the effect of the number of looks on 
#the (h,c) point. Speckle and signal are comparable, since the signal is in [0,1], 
#and the expected value of the speckle is 1. D = 6, looks = c(z, 500, 100, 1, 8, 3)
p = cotas(factorial(6)^2)
p = p + geom_point(data = Entropy.Complexity, aes(x = H, y = C, color = Speckle.Looks), size = 2.5, alpha = .8) + 
  geom_label_repel(data = Entropy.Complexity, aes(x = H, y = C, label = waves.types), 
                   segment.size = 0.05, min.segment.length = 0, force = 18, alpha = 1) +
  xlim(limits=c(0.3, 0.6)) + ylim(limits=c(0.3, 0.6)) + 
  theme_few(base_size = 18, base_family = "serif")  + 
  theme(plot.title = element_text(hjust=0.5), legend.position="none") +
  #ggtitle(TeX('\\textit{H} $\ \\times $\ \\textit{C Plane}')) +
  labs(x = TeX("\\textit{H}"), y = TeX("\\textit{C}")) 

pdf("p1.pdf", width = 8, height = 6) 
p
dev.off() 

############################################## Analysis 2 ################################################

hilbertcurve = unlist(read.table("../Data/Hilbert/HilbertCurves.txt")) + 1
Entropy.Complexity.2 = data.frame(H = length(12), C = length(12), "Speckle Looks" = as.factor(waves.types))

x <- y <- seq(-2*pi, 2*pi, length.out = 128)
deterministic <- function(x, y) {sin(4*x+.5*y)}
z <- outer(x,y,deterministic)
range(z) # it is in [-1, 1], let's scale it to [0,1]
z <- normalize(z)
ts = z[hilbertcurve]/max(z[hilbertcurve])
g = transition.graph.weight(ts, 6, 1)
Entropy.Complexity.2[1,1] = shannonNormalized(as.vector(g))
Entropy.Complexity.2[1,2] = Ccomplexity(as.vector(g))

a = 2
waves.types = 0
looks = c(1, seq(from = 50, to = 500, by = 50))
for(l in looks){
  set.seed(1234567890) # for reproducibility
  SpeckleLooks <- matrix(rgamma(128*128, shape = l, rate = l), nrow = 128)
  waves.types = c(waves.types, l)
  zLook <- z * t(SpeckleLooks)
  ts = zLook[hilbertcurve]/max(zLook[hilbertcurve])
  g = transition.graph.weight(ts, 6, 1)
  Entropy.Complexity.2[a, 1] = shannonNormalized(as.vector(g))
  Entropy.Complexity.2[a, 2] = Ccomplexity(as.vector(g))
  a = a + 1
}
waves.types = as.factor(waves.types)
Entropy.Complexity.2$Speckle.Looks = waves.types

##This figure illustrates the effect of the number of looks on 
#the (h,c) point. Speckle and signal are comparable, since the signal is in [0,1], 
#and the expected value of the speckle is 1. D = 6, looks = c(z, 500, 100, 1, 8, 3)
p = cotas(factorial(6)^2)
p = p + geom_point(data = Entropy.Complexity.2, aes(x = H, y = C, color = Speckle.Looks), size = 1.5, alpha = .4) + 
  geom_label_repel(data = Entropy.Complexity.2, aes(x = H, y = C, label = waves.types), 
                   segment.size = 0.1, min.segment.length = 0, force = 18, alpha = 1) +
  ggtitle(TeX("\\textit{H} $ \\times $ \\textit{C Plane}")) +
  labs(x = TeX("\\textit{H}"), y = TeX("\\textit{C}"))  +
  xlim(limits=c(0.3, 0.6)) + ylim(limits=c(0.3, 0.6)) + 
  theme_few(base_size = 18, base_family = "serif")  + 
  theme(plot.title = element_text(hjust=0.5), legend.position="none")

pdf("p2.pdf", width = 8, height = 6) 
p
dev.off() 

############################################## Analysis 3 ################################################

hilbertcurve = unlist(read.table("../Data/Hilbert/HilbertCurves.txt")) + 1
Entropy.Complexity.3 = data.frame(H = length(12), C = length(12), "Speckle Looks" = as.factor(waves.types))

x <- y <- seq(-2*pi, 2*pi, length.out = 128)
deterministic <- function(x, y) {sin(4*x+.5*y)}
z <- outer(x,y,deterministic)
range(z) # it is in [-1, 1], let's scale it to [0,1]
z <- normalize(z)
ts = z[hilbertcurve]/max(z[hilbertcurve])
g = transition.graph.weight(ts, 6, 1)
Entropy.Complexity.3[1,1] = shannonNormalized(as.vector(g))
Entropy.Complexity.3[1,2] = Ccomplexity(as.vector(g))

a = 2
waves.types = 0
looks = c(1, seq(from = 50, to = 500, by = 50))
for(l in looks){
  set.seed(1234567890) # for reproducibility
  SpeckleLooks <- matrix(rgamma(128*128, shape = l, rate = l), nrow = 128)
  waves.types = c(waves.types, l)
  zLook <- t(z * SpeckleLooks)
  ts = zLook[hilbertcurve]/max(zLook[hilbertcurve])
  g = transition.graph.weight(ts, 6, 1)
  Entropy.Complexity.3[a, 1] = shannonNormalized(as.vector(g))
  Entropy.Complexity.3[a, 2] = Ccomplexity(as.vector(g))
  a = a + 1
}
waves.types = as.factor(waves.types)
Entropy.Complexity.3$Speckle.Looks = waves.types

##This figure illustrates the effect of the number of looks on 
#the (h,c) point. Speckle and signal are comparable, since the signal is in [0,1], 
#and the expected value of the speckle is 1. D = 6, looks = c(z, 500, 100, 1, 8, 3)
p = cotas(factorial(6)^2)
p = p + geom_point(data = Entropy.Complexity.3, aes(x = H, y = C, color = Speckle.Looks), size = 1.5, alpha = .4) + 
  geom_label_repel(data = Entropy.Complexity.3, aes(x = H, y = C, label = waves.types), 
                   segment.size = 0.1, min.segment.length = 0, force = 18, alpha = 1) +
  #ggtitle(TeX("\\textit{H} $ \\times $ \\textit{C Plane}")) +
  labs(x = TeX("\\textit{H}"), y = TeX("\\textit{C}"))  +
  xlim(limits=c(0.3, 0.6)) + ylim(limits=c(0.3, 0.6)) + 
  theme_few(base_size = 18, base_family = "serif")  + 
  theme(plot.title = element_text(hjust=0.5), legend.position="none")

pdf("p3.pdf", width = 8, height = 6) 
p
dev.off() 

## Rescale z -------------------------------------------------------------------
hilbertcurve = unlist(read.table("../Data/Hilbert/HilbertCurves.txt")) + 1

x = y = seq(-2*pi, 2*pi, length.out = 128)
deterministic <- function(x, y) {sin(4*x+.5*y)}
z = outer(x, y, deterministic)

L = c(1, 250, 500)
alpha = c(0, 0.5, 0.9)
Entropy.Complexity = data.frame(H = length(length(L) * length(alpha)), C = length(length(L) * length(alpha)),
                                Look = length(length(L) * length(alpha)), Alpha = length(length(L) * length(alpha)))

i = 1
for(l in L){
  set.seed(1234567890)
  SpeckleLooks = matrix(rgamma(128*128, shape = l, rate = l), nrow = 128)
  for(a in alpha){
    zz = rescale(z, to = c(a, 1))
    zLook <- zz * t(SpeckleLooks)
    ts = zLook[hilbertcurve]/max(zLook[hilbertcurve])
    g = transition.graph.weight(ts, 6, 1)
    Entropy.Complexity[i, 1] = shannonNormalized(as.vector(g))
    Entropy.Complexity[i, 2] = Ccomplexity(as.vector(g))
    i = i + 1
  }
}
Entropy.Complexity$Alpha = as.factor(rep(alpha, length(L)))
Entropy.Complexity$Look = as.factor(c(rep('L = 1', length(alpha)), rep('L = 250', length(alpha)), rep('L = 500', length(alpha))))

p = cotas(factorial(6)^2)
p = p + geom_point(data = Entropy.Complexity, aes(x = H, y = C, color = Alpha), size = 1.5, alpha = .8) +
  geom_label_repel(data = Entropy.Complexity, aes(x = H, y = C, label = Alpha), 
                   segment.size = 0.1, min.segment.length = 0, force = 18, alpha = 1) +
  theme_few(base_size = 18, base_family = "serif")  + 
  theme(plot.title = element_text(hjust=0.5), legend.position="none") +
  ggtitle(TeX("\\textit{H} $ \\times $ \\textit{C Plane}")) +
  labs(x = TeX("\\textit{H}"), y = TeX("\\textit{C}"))  +
  facet_grid(cols = vars(Look)) +
  xlim(limits=c(0, 0.6)) 

pdf("p_L_A.pdf", width = 8, height = 6) 
p
dev.off() 