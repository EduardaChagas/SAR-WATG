
#Plota as cotas do Plano HC
cotas <- function(dimension){
  c1x = readingMPR(dimension,1)
  c1y = readingMPR(dimension,2)
  c2x = readingMPR(dimension,3)
  c2y = readingMPR(dimension,4)
  
  p = qplot(xlab=expression(H), ylab=expression(C)) +
    geom_line(aes(x=c2x, y=c2y), size=1.5, color="gray") +
    geom_line(aes(x=c1x, c1y), size=1.5, color="gray") + 
    theme_few(base_size = 18, base_family = "serif")  + 
    theme(plot.title = element_text(hjust=0.5)) + 
    scale_colour_few("Dark")
  print(p)
  return(p)
}

#Vai adicionando os pontos de Entropia e Complexidade no Plano HC
###IMPORTANTE: ESTA FUNÇÃO É ACUMULATIVA, SEMPRE IRÁ ADIOCIONAR PONTOS. PARA REFAZER ALGUMA ADIÇÃO É NECESSÁRIO CHAMAR NOVAMENTE A FUNÇÃO "cotas(dimension)"
HCPlane <- function(p, Entropy.Complexity, dimension, want_dotted){
  if(want_dotted == 1 && dim(Entropy.Complexity)[1] > 1){
    init = 1
    end = dim(Entropy.Complexity)[1]
    p = p + 
      geom_segment(aes(x=Entropy.Complexity$H[init:(end - 1)],
                       xend=Entropy.Complexity$H[(init + 1):end],
                       y=Entropy.Complexity$C[init:(end - 1)],
                       yend=Entropy.Complexity$C[(init + 1):end]), linetype="dotted")
  }
  p = p +
    geom_point(aes(x = Entropy.Complexity$H, y = Entropy.Complexity$C),size = 2)
    #xlim(limits=c(min(Entropy.Complexity$H), max(Entropy.Complexity$H))) + ylim(limits=c(min(Entropy.Complexity$C), max(Entropy.Complexity$C)))  
  print(p)
  return(p)
}
p <- cotas(factorial(dimension)^2)
pHC <- HCPlane(p, Entropy.Complexity[1,], 6,1)
pHC <- HCPlane(p, Entropy.Complexity[2,], 6,1)
pHC <- HCPlane(p, Entropy.Complexity[3,], 6,1)
pHC <- HCPlane(p, Entropy.Complexity[4,], 6,1)
pHC <- HCPlane(p, Entropy.Complexity[5,], 6,1)
