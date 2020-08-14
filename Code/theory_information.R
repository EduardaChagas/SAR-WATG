###################################################################################
# TheoryInformation.R 
#
# Implementation of theory information descriptors 
#
# Author: Eduarda Chagas
# Date : Set 2019
# Contact: eduarda.chagas@dcc.ufmg.br
####################################################################################

if(!require(ggplot2)) install.packages("ggplot2")

shannonEntropy <- function(p){
  h <- p * log(p)
  h[is.nan(h)] <- 0
  return(-sum(h))
}

shannonNormalized <- function(p){
  h = (shannonEntropy(p)/log(length(p)))
  h[is.nan(h)] <- 0
  return(h)
}

jensenDivergence<-function(p){
  cc = rep(1/length(p),length(p))
  s_p = shannonEntropy(p)
  s_q = shannonEntropy(cc)
  s_pq = shannonEntropy((p+cc)/2)
  divergence = sum(s_pq - (s_p/2) - (s_q/2))
  return(divergence)
}

constant <- function(p){
  k = (0.5)/length(p)
  a1 = (0.5 + k) * log(0.5 + k)
  a2 = (length(p) - 1) * k * log(k)
  a3 = (1 - 0.5) * log(length(p))
  b = -1/(a1 + a2 + a3)
  return(b)
}

Ccomplexity<-function(p){
  cc <- jensenDivergence(p) * constant(p) * shannonNormalized(p)
  return(cc)
}

cotas <- function(dimension){
  c1x = readingMPR(dimension,1)
  c1y = readingMPR(dimension,2)
  c2x = readingMPR(dimension,3)
  c2y = readingMPR(dimension,4)
  
  cotas.1xy = data.frame("c1x" = c1x, "c1y" = c1y)
  cotas.2xy = data.frame("c2x" = c2x, "c2y" = c2y)
  
  p = ggplot(cotas.1xy, aes(c1x, c1y)) + geom_line(size=0.5, color="gray") +
    geom_line(aes(x=c2x, y=c2y), cotas.2xy, size=0.5, color="gray") +
    theme(plot.title = element_text(hjust=0.5)) 
  return(p)
}

readingMPR<-function(dimension,option=0){
  if(dimension == 3){ 
    continua = "../Data/trozos/continuaN6.txt"
    trozo = "../Data/trozos/trozosN6.txt"
  }
  if(dimension == 4){ 
    continua = "../Data/trozos/continuaN24.txt"
    trozo = "../Data/trozos/trozosN24.txt"
  }
  if(dimension == 5){ 
    continua = "../Data/trozos/continuaN120.txt"
    trozo = "../Data/trozos/trozosN120.txt"
  }
  if(dimension == 6){ 
    continua = "../Data/trozos/continuaN720.txt"
    trozo = "../Data/trozos/trozosN720.txt"
  }
  if(dimension == 36){ 
    continua = "../Data/trozos/continuaN36.txt"
    trozo = "../Data/trozos/trozosN36.txt"
  }
  if(dimension == 576){ 
    continua = "../Data/trozos/continuaN576.txt"
    trozo = "../Data/trozos/trozosN576.txt"
  }
  if(dimension == 14400){ 
    continua = "../Data/trozos/continuaN14400.txt"
    trozo = "../Data/trozos/trozosN14400.txt"
  }
  if(dimension == 518400){ 
    continua = "../Data/trozos/continuaN518400.txt"
    trozo = "../Data/trozos/trozosN518400.txt"
  }
  curva1x = read.table(continua, stringsAsFactors=FALSE, fileEncoding="latin1")[,1]
  if(option==1) return(curva1x)
  curva1y = read.table(continua, stringsAsFactors=FALSE, fileEncoding="latin1")[,2]
  if(option==2) return(curva1y)
  curva2x = read.table(trozo, stringsAsFactors=FALSE, fileEncoding="latin1")[,1]
  if(option==3) return(curva2x)
  curva2y = read.table(trozo, stringsAsFactors=FALSE, fileEncoding="latin1")[,2]
  if(option==4) return(curva2y)
}