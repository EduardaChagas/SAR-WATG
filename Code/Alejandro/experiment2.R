
Entropy.Complexity = data.frame(H = length(3), C = length(3))

set.seed(1234567890) # for reproducibility
Speckle3Looks <- matrix(rgamma(128*128, shape=3, scale=1/3), nrow = 128)

set.seed(1234567890) # for reproducibility
Speckle3Looks.01 <- matrix(rgamma(128*128, shape=3, scale=(1/3 * .01)), nrow = 128)


ts = z[hilbertcurve]/max(z[hilbertcurve])
g = transition.graph.weight(ts, dimension, 1)
Entropy.Complexity[1,1] = shannonNormalized(as.vector(g))
Entropy.Complexity[1,2] = Ccomplexity(as.vector(g))

z.1 <- z * Speckle3Looks 
ts = z.1[hilbertcurve]/max(z.1[hilbertcurve])
g = transition.graph.weight(ts, dimension, 1)
Entropy.Complexity[2,1] = shannonNormalized(as.vector(g))
Entropy.Complexity[2,2] = Ccomplexity(as.vector(g))

z.2 <- z * Speckle3Looks.01 
ts = z.1[hilbertcurve]/max(z.1[hilbertcurve])
g = transition.graph.weight(ts, dimension, 1)
Entropy.Complexity[3,1] = shannonNormalized(as.vector(g))
Entropy.Complexity[3,2] = Ccomplexity(as.vector(g))

waves.types = c("z", "Speckle3Looks", "Speckle3Looks.01")
Entropy.Complexity = data.frame(H = Entropy.Complexity$H, C = Entropy.Complexity$C, names = as.factor(waves.types))


x = z
y = Speckle3Looks
y2 = (y - min(y)) / (max(y) - min(y))
b = 1
for(i in 1:3){
  z.alpha.1 <- x * i * y2
  ts = z.alpha.1[hilbertcurve]/max(z.alpha.1[hilbertcurve])
  g = transition.graph.weight(ts, dimension, 1)
  Entropy.Complexity[b,1] = shannonNormalized(as.vector(g))
  Entropy.Complexity[b,2] = Ccomplexity(as.vector(g))
  print(b)
  b = b + 1
}

