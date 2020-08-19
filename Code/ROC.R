# Load some packages:
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(ggthemes)) install.packages("ggthemes")
if(!require(caret)) install.packages("caret")
if(!require(EnvStats)) install.packages("EnvStats")
if(!require(MLmetrics)) install.packages("MLmetrics")
if(!require(mltest)) install.packages("mltest")
if(!require(ggrepel)) install.packages("ggrepel")


analysis.metrics <- function(){
  
  n = c(3,4,5,6) #Dimension parameter
  tal = c(1,2,3,4,5) #Delay parameter
  
  split = 0.85
  n.total = 200
  set.seed(12345678)
  regions = c(rep("Forest",40), rep("Sea",80), rep("Urban", 40), rep("Pasture", 40))
  ctrl = trainControl(method="repeatedcv", number = 10, repeats = 10, savePredictions = T)
  
  Entropy.Complexity = data.frame("Entropy" = numeric(n.total), 
                                  "Complexity" = numeric(n.total),
                                  "Region" = character(n.total), 
                                  stringsAsFactors=FALSE)
  
  metrics.calculated = data.frame("Accuracy" = numeric(20),
                                  "Recall" = numeric(20),
                                  "Precision" = numeric(20),
                                  "F1" = numeric(20),
                                  "FPR" = numeric(20),
                                  "sensitivity" = numeric(20),
                                  stringsAsFactors=FALSE)
  
  a = 1
  for(t in tal){
    for(d in n){
      print(paste0("Rodada ", a))
      ctrl = trainControl(method="repeatedcv", number = 10, repeats = 10, savePredictions = T)
      
      Entropy.Complexity.csv = read.csv(paste('../Data/EntropyComplexityWATGD', d, 'T', t, '.csv', sep = ""))
      Entropy.Complexity$Entropy = Entropy.Complexity.csv[1:200, 1]
      Entropy.Complexity$Complexity = Entropy.Complexity.csv[1:200, 2]
      Entropy.Complexity$Region = regions
      
      trainIndex = createDataPartition(Entropy.Complexity$Region, p = split, list = FALSE)
      
      x = data.frame(Entropy.Complexity$Entropy[trainIndex], Entropy.Complexity$Complexity[trainIndex])
      y = factor(Entropy.Complexity$Region[trainIndex])
      
      x_validation = data.frame("Entropy" = Entropy.Complexity$Entropy[-trainIndex], "Complexity" = Entropy.Complexity$Complexity[-trainIndex])
      y_validation = factor(Entropy.Complexity$Region[-trainIndex])
      
      Entropy.Complexity.new = data.frame("Entropy" = Entropy.Complexity$Entropy[trainIndex],
                                      "Complexity" = Entropy.Complexity$Complexity[trainIndex],
                                      "Region" = Entropy.Complexity$Region[trainIndex],
                                      stringsAsFactors=FALSE)
      
      knnFit = train(Region~., data = Entropy.Complexity.new, method = "knn",
                     trControl = ctrl,
                     preProcess = c("center","scale"),
                     tuneLength = 20)
      
      pred = predict(knnFit, newdata = x_validation)
      
      metrics.calculated$Accuracy[a] = Accuracy(pred, y_validation)
      metrics.calculated$Recall[a] = Recall(pred, y_validation)
      metrics.calculated$Precision[a] = Precision(pred, y_validation)
      metrics.calculated$F1[a] = F1_Score(pred, y_validation)
      me = ml_test(pred, y_validation, output.as.table = FALSE)
      metrics.calculated$FPR[a] =  1 - (sum(me$specificity)/4)
      cm = confusionMatrix(table(pred, y_validation))
      metrics.calculated$sensitivity[a] = sum(diag(cm$table))/sum(cm$table)
      a = a + 1
    }
  }
  write.csv(metrics.calculated, "metrics.calculated.csv")
}


plot.dimension.tau <- function(){

  D = c(rep(3,5), rep(4,5), rep(5,5), rep(6,5))
  point.shape = c(rep(15,5), rep(16,5), rep(17,5), rep(18,5))
  line.shape = c(rep(2,5), rep(3,5), rep(4,5), rep(5,5))
  tau = rep(c(1,2,3,4,5), 4)
  
  D = factor(D)
  
  
  metrics.calculated.csv = read.csv("../Data/metrics.calculated.csv")

  rainbow_colors = palette(c("#A80055",
                             "#B18FCF",
                             "#2C2C34",
                             "#00FF29",
                             "#EA782C",
                             "#FF3030",
                             "#00FFF7",
                             "#153243",
                             "#000000",
                             "#E8FCCF",
                             "#EFE940",
                             "#5A43EF",
                             "#27EBAF",
                             "#C06E52",
                             "#3C1518",
                             "#EF798A",
                             "#355D34",
                             "#69140E",
                             "#AD6634",
                             "#311B40"))
  legend.names = rep(c(bquote(tau == 1), 
                       bquote(tau == 2), 
                       bquote(tau == 3), 
                       bquote(tau == 4), 
                       bquote(tau == 5)),  4)

     ROC.curve =  ggplot(data = metrics.calculated.csv,
                         aes(x = FPR, y = Recall,
                             group = D, shape = D, color = D)) + 
       geom_label_repel(aes(label = legend.names), box.padding   = 0.65, point.padding = 0.5, segment.color = 'grey50', size = 2, parse = TRUE) +
       geom_point(shape = point.shape, size = 3) + scale_color_manual(values = rainbow_colors) +
       ylab("True Positive Rate") +
       xlab("False Positive Rate") +
       scale_shape_identity() +
       theme_few(base_size = 12, base_family = "serif")  +
       theme(plot.title = element_text(hjust=0.5))
     return(ROC.curve)
}

#analysis.metrics()

pdf("ROC.pdf", width = 6, height = 4)
ROC.curve = plot.dimension.tau()
print(ROC.curve)
dev.off()
