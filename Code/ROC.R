# Load some packages:
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(ggthemes)) install.packages("ggthemes")
if(!require(caret)) install.packages("caret")
if(!require(EnvStats)) install.packages("EnvStats")
if(!require(MLmetrics)) install.packages("MLmetrics")
if(!require(mltest)) install.packages("mltest")
if(!require(ggrepel)) install.packages("ggrepel")


analysis.metrics <- function(){
  
  regions = c(rep("Forest",40), rep("Sea",80), rep("Urban", 40))
  split = 0.85
  set.seed(12345678)
  n.total = 160
  
  Entropy.Complexity.csv = read.csv(file="../Data/EntropyComplexityWATG.csv",
                                    header=TRUE, sep=",")
  
  
  ctrl = trainControl(method="repeatedcv", number = 10, repeats = 10, savePredictions = T)
  
  metrics.calculated = data.frame("Accuracy" = numeric(20),
                                  "Recall" = numeric(20),
                                  "Precision" = numeric(20),
                                  "F1" = numeric(20),
                                  "FPR" = numeric(20),
                                  "sensitivity" = numeric(20),
                                  stringsAsFactors=FALSE)
  
  #############################################################################################
  
  Entropy.Complexity.D3T1 = data.frame("Entropy" = numeric(n.total),
                                       "Complexity" = numeric(n.total),
                                       "Region" = character(n.total),
                                       stringsAsFactors=FALSE)
  
  Entropy.Complexity.D3T1$Entropy = Entropy.Complexity.csv[1:n.total, 1]
  Entropy.Complexity.D3T1$Complexity = Entropy.Complexity.csv[1:n.total, 2]
  Entropy.Complexity.D3T1$Region = regions
  
    trainIndex = createDataPartition(Entropy.Complexity.D3T1$Region, p = split, list = FALSE)
  
  x1 = data.frame(Entropy.Complexity.D3T1$Entropy[trainIndex], Entropy.Complexity.D3T1$Complexity[trainIndex])
  y1 = factor(Entropy.Complexity.D3T1$Region[trainIndex])
  
  x_validation1 = data.frame("Entropy" = Entropy.Complexity.D3T1$Entropy[-trainIndex], "Complexity" = Entropy.Complexity.D3T1$Complexity[-trainIndex])
  y_validation1 = factor(Entropy.Complexity.D3T1$Region[-trainIndex])
  
  Entropy.Complexity.D3T1 = data.frame("Entropy" = Entropy.Complexity.D3T1$Entropy[trainIndex],
                                       "Complexity" = Entropy.Complexity.D3T1$Complexity[trainIndex],
                                       "Region" = Entropy.Complexity.D3T1$Region[trainIndex],
                                       stringsAsFactors=FALSE)
  
  knnFit = train(Region~., data = Entropy.Complexity.D3T1, method = "knn",
                 trControl = ctrl,
                 preProcess = c("center","scale"),
                 tuneLength = 20)
  
  pred = predict(knnFit, newdata = x_validation1)
  
  metrics.calculated$Accuracy[1] = Accuracy(y_pred = pred, y_true = y_validation1)
  metrics.calculated$Recall[1] = Recall(y_pred = pred, y_true = y_validation1)
  metrics.calculated$Precision[1] = Precision(y_pred = pred, y_true = y_validation1)
  metrics.calculated$F1[1] = F1_Score(y_pred = pred, y_true = y_validation1)
  me = ml_test(pred, y_validation1, output.as.table = FALSE)
  metrics.calculated$FPR[1] =  1 - (sum(me$specificity)/3)
  cm = confusionMatrix(table(pred, y_validation1))
  metrics.calculated$sensitivity[1] = sum(diag(cm$table))/sum(cm$table)
  
  #############################################################################################
  ctrl = trainControl(method="repeatedcv", number = 10, repeats = 10, savePredictions = T)
  
  Entropy.Complexity.D3T2 = data.frame("Entropy" = numeric(n.total),
                                       "Complexity" = numeric(n.total),
                                       "Region" = character(n.total),
                                       stringsAsFactors=FALSE)
  
  Entropy.Complexity.D3T2$Entropy = Entropy.Complexity.csv[(n.total + 1):(2*n.total), 1]
  Entropy.Complexity.D3T2$Complexity = Entropy.Complexity.csv[(n.total + 1):(2*n.total), 2]
  Entropy.Complexity.D3T2$Region = regions
  
  trainIndex = createDataPartition(Entropy.Complexity.D3T2$Region, p = split, list = FALSE)
  
  x2 = data.frame(Entropy.Complexity.D3T2$Entropy[trainIndex], Entropy.Complexity.D3T2$Complexity[trainIndex])
  y2 = factor(Entropy.Complexity.D3T2$Region[trainIndex])
  
  x_validation2 = data.frame("Entropy" = Entropy.Complexity.D3T2$Entropy[-trainIndex], "Complexity" = Entropy.Complexity.D3T2$Complexity[-trainIndex])
  y_validation2 = factor(Entropy.Complexity.D3T2$Region[-trainIndex])
  
  Entropy.Complexity.D3T2 = data.frame("Entropy" = Entropy.Complexity.D3T2$Entropy[trainIndex],
                                       "Complexity" = Entropy.Complexity.D3T2$Complexity[trainIndex],
                                       "Region" = Entropy.Complexity.D3T2$Region[trainIndex],
                                       stringsAsFactors=FALSE)
  
  knnFit2 = train(Region~., data = Entropy.Complexity.D3T2, method = "knn",
                  trControl = ctrl,
                  preProcess = c("center","scale"),
                  tuneLength = 20)
  
  pred = predict(knnFit2, newdata = x_validation2)
  
  metrics.calculated$Accuracy[2] = Accuracy(y_pred = pred, y_true = y_validation2)
  metrics.calculated$Recall[2] = Recall(y_pred = pred, y_true = y_validation2)
  metrics.calculated$Precision[2] = Precision(y_pred = pred, y_true = y_validation2)
  metrics.calculated$F1[2] = F1_Score(y_pred = pred, y_true = y_validation2)
  me = ml_test(pred, y_validation2, output.as.table = FALSE)
  metrics.calculated$FPR[2] = 1 - (sum(me$specificity)/3)
  cm = confusionMatrix(table(pred, y_validation2))
  metrics.calculated$sensitivity[2] = sum(diag(cm$table))/sum(cm$table)
  
  #############################################################################################
  ctrl = trainControl(method="repeatedcv", number = 10, repeats = 10, savePredictions = T)
  
  Entropy.Complexity.D3T3 = data.frame("Entropy" = numeric(n.total),
                                       "Complexity" = numeric(n.total),
                                       "Region" = character(n.total),
                                       stringsAsFactors=FALSE)
  
  Entropy.Complexity.D3T3$Entropy = Entropy.Complexity.csv[((2*n.total) + 1):(3*n.total), 1]
  Entropy.Complexity.D3T3$Complexity = Entropy.Complexity.csv[((2*n.total) + 1):(3*n.total), 2]
  Entropy.Complexity.D3T3$Region = regions
  
  trainIndex = createDataPartition(Entropy.Complexity.D3T3$Region, p = split, list = FALSE)
  
  x3 = data.frame(Entropy.Complexity.D3T3$Entropy[trainIndex], Entropy.Complexity.D3T3$Complexity[trainIndex])
  y3 = factor(Entropy.Complexity.D3T3$Region[trainIndex])
  
  x_validation3 = data.frame("Entropy" = Entropy.Complexity.D3T3$Entropy[-trainIndex], "Complexity" = Entropy.Complexity.D3T3$Complexity[-trainIndex])
  y_validation3 = factor(Entropy.Complexity.D3T3$Region[-trainIndex])
  
  Entropy.Complexity.D3T3 = data.frame("Entropy" = Entropy.Complexity.D3T3$Entropy[trainIndex],
                                       "Complexity" = Entropy.Complexity.D3T3$Complexity[trainIndex],
                                       "Region" = Entropy.Complexity.D3T3$Region[trainIndex],
                                       stringsAsFactors=FALSE)
  
  knnFit3 = train(Region~., data = Entropy.Complexity.D3T3, method = "knn",
                  trControl = ctrl,
                  preProcess = c("center","scale"),
                  tuneLength = 20)
  
  pred = predict(knnFit3, newdata = x_validation3)
  cm = confusionMatrix(data = pred, reference = y_validation3, mode = "prec_recall")
  
  metrics.calculated$Accuracy[3] = Accuracy(y_pred = pred, y_true = y_validation3)
  metrics.calculated$Recall[3] = Recall(y_pred = pred, y_true = y_validation3)
  metrics.calculated$Precision[3] = Precision(y_pred = pred, y_true = y_validation3)
  metrics.calculated$F1[3] = F1_Score(y_pred = pred, y_true = y_validation3)
  me = ml_test(pred, y_validation3, output.as.table = FALSE)
  metrics.calculated$FPR[3] =  1 - (sum(me$specificity)/3)
  cm = confusionMatrix(table(pred, y_validation3))
  metrics.calculated$sensitivity[3] = sum(diag(cm$table))/sum(cm$table)
  
  #############################################################################################
  ctrl = trainControl(method="repeatedcv", number = 10, repeats = 10, savePredictions = T)
  
  Entropy.Complexity.D3T4 = data.frame("Entropy" = numeric(n.total),
                                       "Complexity" = numeric(n.total),
                                       "Region" = character(n.total),
                                       stringsAsFactors=FALSE)
  
  Entropy.Complexity.D3T4$Entropy = Entropy.Complexity.csv[((3*n.total) + 1):(4*n.total), 1]
  Entropy.Complexity.D3T4$Complexity = Entropy.Complexity.csv[((3*n.total) + 1):(4*n.total), 2]
  Entropy.Complexity.D3T4$Region = regions
  
  trainIndex = createDataPartition(Entropy.Complexity.D3T4$Region, p = split, list = FALSE)
  
  x4 = data.frame(Entropy.Complexity.D3T4$Entropy[trainIndex], Entropy.Complexity.D3T4$Complexity[trainIndex])
  y4 = factor(Entropy.Complexity.D3T4$Region[trainIndex])
  
  x_validation4 = data.frame("Entropy" = Entropy.Complexity.D3T4$Entropy[-trainIndex], "Complexity" = Entropy.Complexity.D3T4$Complexity[-trainIndex])
  y_validation4 = factor(Entropy.Complexity.D3T4$Region[-trainIndex])
  
  Entropy.Complexity.D3T4 = data.frame("Entropy" = Entropy.Complexity.D3T4$Entropy[trainIndex],
                                       "Complexity" = Entropy.Complexity.D3T4$Complexity[trainIndex],
                                       "Region" = Entropy.Complexity.D3T4$Region[trainIndex],
                                       stringsAsFactors=FALSE)
  
  knnFit4 = train(Region~., data = Entropy.Complexity.D3T4, method = "knn",
                  trControl = ctrl,
                  preProcess = c("center","scale"),
                  tuneLength = 20)
  
  pred = predict(knnFit4, newdata = x_validation4)
  
  metrics.calculated$Accuracy[4] = Accuracy(y_pred = pred, y_true = y_validation4)
  metrics.calculated$Recall[4] = Recall(y_pred = pred, y_true = y_validation4)
  metrics.calculated$Precision[4] = Precision(y_pred = pred, y_true = y_validation4)
  metrics.calculated$F1[4] = F1_Score(y_pred = pred, y_true = y_validation4)
  me = ml_test(pred, y_validation4, output.as.table = FALSE)
  metrics.calculated$FPR[4] =  1 - (sum(me$specificity)/3)
  cm = confusionMatrix(table(pred, y_validation4))
  metrics.calculated$sensitivity[4] = sum(diag(cm$table))/sum(cm$table)
  
  #############################################################################################
  ctrl = trainControl(method="repeatedcv", number = 10, repeats = 10, savePredictions = T)
  
  Entropy.Complexity.D3T5 = data.frame("Entropy" = numeric(n.total),
                                       "Complexity" = numeric(n.total),
                                       "Region" = character(n.total),
                                       stringsAsFactors=FALSE)
  
  Entropy.Complexity.D3T5$Entropy = Entropy.Complexity.csv[((4*n.total) + 1):(5*n.total), 1]
  Entropy.Complexity.D3T5$Complexity = Entropy.Complexity.csv[((4*n.total) + 1):(5*n.total), 2]
  Entropy.Complexity.D3T5$Region = regions
  
  trainIndex = createDataPartition(Entropy.Complexity.D3T5$Region, p = split, list = FALSE)
  
  x5 = data.frame(Entropy.Complexity.D3T5$Entropy[trainIndex], Entropy.Complexity.D3T5$Complexity[trainIndex])
  y5 = factor(Entropy.Complexity.D3T5$Region[trainIndex])
  
  x_validation5 = data.frame("Entropy" = Entropy.Complexity.D3T5$Entropy[-trainIndex], "Complexity" = Entropy.Complexity.D3T5$Complexity[-trainIndex])
  y_validation5 = factor(Entropy.Complexity.D3T5$Region[-trainIndex])
  
  Entropy.Complexity.D3T5 = data.frame("Entropy" = Entropy.Complexity.D3T5$Entropy[trainIndex],
                                       "Complexity" = Entropy.Complexity.D3T5$Complexity[trainIndex],
                                       "Region" = Entropy.Complexity.D3T5$Region[trainIndex],
                                       stringsAsFactors=FALSE)
  
  knnFit5 = train(Region~., data = Entropy.Complexity.D3T5, method = "knn",
                  trControl = ctrl,
                  preProcess = c("center","scale"),
                  tuneLength = 20)
  
  pred = predict(knnFit5, newdata = x_validation5)
  
  metrics.calculated$Accuracy[5] = Accuracy(pred, y_validation5)
  metrics.calculated$Recall[5] = Recall(pred, y_validation5)
  metrics.calculated$Precision[5] = Precision(pred, y_validation5)
  metrics.calculated$F1[5] = F1_Score(pred, y_validation5)
  me = ml_test(pred, y_validation5, output.as.table = FALSE)
  metrics.calculated$FPR[5] =  1 - (sum(me$specificity)/3)
  cm = confusionMatrix(table(pred, y_validation5))
  metrics.calculated$sensitivity[5] = sum(diag(cm$table))/sum(cm$table)
  
  #############################################################################################
  ctrl = trainControl(method="repeatedcv", number = 10, repeats = 10, savePredictions = T)
  
  Entropy.Complexity.D4T1 = data.frame("Entropy" = numeric(n.total),
                                       "Complexity" = numeric(n.total),
                                       "Region" = character(n.total),
                                       stringsAsFactors=FALSE)
  
  Entropy.Complexity.D4T1$Entropy = Entropy.Complexity.csv[((5*n.total) + 1):(6*n.total), 1]
  Entropy.Complexity.D4T1$Complexity = Entropy.Complexity.csv[((5*n.total) + 1):(6*n.total), 2]
  Entropy.Complexity.D4T1$Region = regions
  
  trainIndex = createDataPartition(Entropy.Complexity.D4T1$Region, p = split, list = FALSE)
  
  x6 = data.frame(Entropy.Complexity.D4T1$Entropy[trainIndex], Entropy.Complexity.D4T1$Complexity[trainIndex])
  y6 = factor(Entropy.Complexity.D4T1$Region[trainIndex])
  
  x_validation6 = data.frame("Entropy" = Entropy.Complexity.D4T1$Entropy[-trainIndex], "Complexity" = Entropy.Complexity.D4T1$Complexity[-trainIndex])
  y_validation6 = factor(Entropy.Complexity.D4T1$Region[-trainIndex])
  
  Entropy.Complexity.D4T1 = data.frame("Entropy" = Entropy.Complexity.D4T1$Entropy[trainIndex],
                                       "Complexity" = Entropy.Complexity.D4T1$Complexity[trainIndex],
                                       "Region" = Entropy.Complexity.D4T1$Region[trainIndex],
                                       stringsAsFactors=FALSE)
  
  knnFit6 = train(Region~., data = Entropy.Complexity.D4T1, method = "knn",
                  trControl = ctrl,
                  preProcess = c("center","scale"),
                  tuneLength = 20)
  
  pred = predict(knnFit6, newdata = x_validation6)
  
  metrics.calculated$Accuracy[6] = Accuracy(pred, y_validation6)
  metrics.calculated$Recall[6] = Recall(pred, y_validation6)
  metrics.calculated$Precision[6] = Precision(pred, y_validation6)
  metrics.calculated$F1[6] = F1_Score(pred, y_validation6)
  me = ml_test(pred, y_validation6, output.as.table = FALSE)
  metrics.calculated$FPR[6] =  1 - (sum(me$specificity)/3)
  cm = confusionMatrix(table(pred, y_validation6))
  metrics.calculated$sensitivity[6] = sum(diag(cm$table))/sum(cm$table)
  
  #############################################################################################
  ctrl = trainControl(method="repeatedcv", number = 10, repeats = 10, savePredictions = T)
  
  Entropy.Complexity.D4T2 = data.frame("Entropy" = numeric(n.total),
                                       "Complexity" = numeric(n.total),
                                       "Region" = character(n.total),
                                       stringsAsFactors=FALSE)
  
  Entropy.Complexity.D4T2$Entropy = Entropy.Complexity.csv[((6*n.total) + 1):(7*n.total), 1]
  Entropy.Complexity.D4T2$Complexity = Entropy.Complexity.csv[((6*n.total) + 1):(7*n.total), 2]
  Entropy.Complexity.D4T2$Region = regions
  
  trainIndex = createDataPartition(Entropy.Complexity.D4T2$Region, p = split, list = FALSE)
  
  x7 = data.frame(Entropy.Complexity.D4T2$Entropy[trainIndex], Entropy.Complexity.D4T2$Complexity[trainIndex])
  y7 = factor(Entropy.Complexity.D4T2$Region[trainIndex])
  
  x_validation7 = data.frame("Entropy" = Entropy.Complexity.D4T2$Entropy[-trainIndex], "Complexity" = Entropy.Complexity.D4T2$Complexity[-trainIndex])
  y_validation7 = factor(Entropy.Complexity.D4T2$Region[-trainIndex])
  
  Entropy.Complexity.D4T2 = data.frame("Entropy" = Entropy.Complexity.D4T2$Entropy[trainIndex],
                                       "Complexity" = Entropy.Complexity.D4T2$Complexity[trainIndex],
                                       "Region" = Entropy.Complexity.D4T2$Region[trainIndex],
                                       stringsAsFactors=FALSE)
  
  knnFit7 = train(Region~., data = Entropy.Complexity.D4T2, method = "knn",
                  trControl = ctrl,
                  preProcess = c("center","scale"),
                  tuneLength = 20)
  
  pred = predict(knnFit7, newdata = x_validation7)
  
  metrics.calculated$Accuracy[7] = Accuracy(pred, y_validation7)
  metrics.calculated$Recall[7] = Recall(pred, y_validation7)
  metrics.calculated$Precision[7] = Precision(pred, y_validation7)
  metrics.calculated$F1[7] = F1_Score(pred, y_validation7)
  me = ml_test(pred, y_validation7, output.as.table = FALSE)
  metrics.calculated$FPR[7] = 1 - (sum(me$specificity)/3)
  cm = confusionMatrix(table(pred, y_validation7))
  metrics.calculated$sensitivity[7] = sum(diag(cm$table))/sum(cm$table)
  
  #############################################################################################
  ctrl = trainControl(method="repeatedcv", number = 10, repeats = 10, savePredictions = T)
  
  Entropy.Complexity.D4T3 = data.frame("Entropy" = numeric(n.total),
                                       "Complexity" = numeric(n.total),
                                       "Region" = character(n.total),
                                       stringsAsFactors=FALSE)
  
  Entropy.Complexity.D4T3$Entropy = Entropy.Complexity.csv[((7*n.total) + 1):(8*n.total), 1]
  Entropy.Complexity.D4T3$Complexity = Entropy.Complexity.csv[((7*n.total) + 1):(8*n.total), 2]
  Entropy.Complexity.D4T3$Region = regions
  
  trainIndex = createDataPartition(Entropy.Complexity.D4T3$Region, p = split, list = FALSE)
  
  x8 = data.frame(Entropy.Complexity.D4T3$Entropy[trainIndex], Entropy.Complexity.D4T3$Complexity[trainIndex])
  y8 = factor(Entropy.Complexity.D4T3$Region[trainIndex])
  
  x_validation8 = data.frame("Entropy" = Entropy.Complexity.D4T3$Entropy[-trainIndex], "Complexity" = Entropy.Complexity.D4T3$Complexity[-trainIndex])
  y_validation8 = factor(Entropy.Complexity.D4T3$Region[-trainIndex])
  
  Entropy.Complexity.D4T3 = data.frame("Entropy" = Entropy.Complexity.D4T3$Entropy[trainIndex],
                                       "Complexity" = Entropy.Complexity.D4T3$Complexity[trainIndex],
                                       "Region" = Entropy.Complexity.D4T3$Region[trainIndex],
                                       stringsAsFactors=FALSE)
  
  knnFit8 = train(Region~., data = Entropy.Complexity.D4T3, method = "knn",
                  trControl = ctrl,
                  preProcess = c("center","scale"),
                  tuneLength = 20)
  
  pred = predict(knnFit8, newdata = x_validation8)
  
  metrics.calculated$Accuracy[8] = Accuracy(pred, y_validation8)
  metrics.calculated$Recall[8] = Recall(pred, y_validation8)
  metrics.calculated$Precision[8] = Precision(pred, y_validation8)
  metrics.calculated$F1[8] = F1_Score(pred, y_validation8)
  me = ml_test(pred, y_validation8, output.as.table = FALSE)
  metrics.calculated$FPR[8] =  1 - (sum(me$specificity)/3)
  cm = confusionMatrix(table(pred, y_validation8))
  metrics.calculated$sensitivity[8] = sum(diag(cm$table))/sum(cm$table)
  
  #############################################################################################
  ctrl = trainControl(method="repeatedcv", number = 10, repeats = 10, savePredictions = T)
  
  Entropy.Complexity.D4T4 = data.frame("Entropy" = numeric(n.total),
                                       "Complexity" = numeric(n.total),
                                       "Region" = character(n.total),
                                       stringsAsFactors=FALSE)
  
  Entropy.Complexity.D4T4$Entropy = Entropy.Complexity.csv[((8*n.total) + 1):(9*n.total), 1]
  Entropy.Complexity.D4T4$Complexity = Entropy.Complexity.csv[((8*n.total) + 1):(9*n.total), 2]
  Entropy.Complexity.D4T4$Region = regions
  
  trainIndex = createDataPartition(Entropy.Complexity.D4T4$Region, p = split, list = FALSE)
  
  x9 = data.frame(Entropy.Complexity.D4T4$Entropy[trainIndex], Entropy.Complexity.D4T4$Complexity[trainIndex])
  y9 = factor(Entropy.Complexity.D4T4$Region[trainIndex])
  
  x_validation9 = data.frame("Entropy" = Entropy.Complexity.D4T4$Entropy[-trainIndex], "Complexity" = Entropy.Complexity.D4T4$Complexity[-trainIndex])
  y_validation9 = factor(Entropy.Complexity.D4T4$Region[-trainIndex])
  
  Entropy.Complexity.D4T4 = data.frame("Entropy" = Entropy.Complexity.D4T4$Entropy[trainIndex],
                                       "Complexity" = Entropy.Complexity.D4T4$Complexity[trainIndex],
                                       "Region" = Entropy.Complexity.D4T4$Region[trainIndex],
                                       stringsAsFactors=FALSE)
  
  knnFit9 = train(Region~., data = Entropy.Complexity.D4T4, method = "knn",
                  trControl = ctrl,
                  preProcess = c("center","scale"),
                  tuneLength = 20)
  
  pred = predict(knnFit9, newdata = x_validation9)
  
  metrics.calculated$Accuracy[9] = Accuracy(pred, y_validation9)
  metrics.calculated$Recall[9] = Recall(pred, y_validation9)
  metrics.calculated$Precision[9] = Precision(pred, y_validation9)
  metrics.calculated$F1[9] = F1_Score(pred, y_validation9)
  me = ml_test(pred, y_validation9, output.as.table = FALSE)
  metrics.calculated$FPR[9] =  1 - (sum(me$specificity)/3)
  cm = confusionMatrix(table(pred, y_validation9))
  metrics.calculated$sensitivity[9] = sum(diag(cm$table))/sum(cm$table)
  
  #############################################################################################
  ctrl = trainControl(method="repeatedcv", number = 10, repeats = 10, savePredictions = T)
  
  Entropy.Complexity.D4T5 = data.frame("Entropy" = numeric(n.total),
                                       "Complexity" = numeric(n.total),
                                       "Region" = character(n.total),
                                       stringsAsFactors=FALSE)
  
  Entropy.Complexity.D4T5$Entropy = Entropy.Complexity.csv[((9*n.total) + 1):(10*n.total), 1]
  Entropy.Complexity.D4T5$Complexity = Entropy.Complexity.csv[((9*n.total) + 1):(10*n.total), 2]
  Entropy.Complexity.D4T5$Region = regions
  
  trainIndex = createDataPartition(Entropy.Complexity.D4T5$Region, p = split, list = FALSE)
  
  x10 = data.frame(Entropy.Complexity.D4T5$Entropy[trainIndex], Entropy.Complexity.D4T5$Complexity[trainIndex])
  y10 = factor(Entropy.Complexity.D4T5$Region[trainIndex])
  
  x_validation10 = data.frame("Entropy" = Entropy.Complexity.D4T5$Entropy[-trainIndex], "Complexity" = Entropy.Complexity.D4T5$Complexity[-trainIndex])
  y_validation10 = factor(Entropy.Complexity.D4T5$Region[-trainIndex])
  
  Entropy.Complexity.D4T5 = data.frame("Entropy" = Entropy.Complexity.D4T5$Entropy[trainIndex],
                                       "Complexity" = Entropy.Complexity.D4T5$Complexity[trainIndex],
                                       "Region" = Entropy.Complexity.D4T5$Region[trainIndex],
                                       stringsAsFactors=FALSE)
  
  knnFit10 = train(Region~., data = Entropy.Complexity.D4T5, method = "knn",
                   trControl = ctrl,
                   preProcess = c("center","scale"),
                   tuneLength = 20)
  
  pred = predict(knnFit10, newdata = x_validation10)
  
  metrics.calculated$Accuracy[10] = Accuracy(pred, y_validation10)
  metrics.calculated$Recall[10] = Recall(pred, y_validation10)
  metrics.calculated$Precision[10] = Precision(pred, y_validation10)
  metrics.calculated$F1[10] = F1_Score(pred, y_validation10)
  me = ml_test(pred, y_validation10, output.as.table = FALSE)
  metrics.calculated$FPR[10] =  1 - (sum(me$specificity)/3)
  cm = confusionMatrix(table(pred, y_validation10))
  metrics.calculated$sensitivity[10] = sum(diag(cm$table))/sum(cm$table)
  
  #############################################################################################
  ctrl = trainControl(method="repeatedcv", number = 10, repeats = 10, savePredictions = T)
  
  Entropy.Complexity.D5T1 = data.frame("Entropy" = numeric(n.total),
                                       "Complexity" = numeric(n.total),
                                       "Region" = character(n.total),
                                       stringsAsFactors=FALSE)
  
  Entropy.Complexity.D5T1$Entropy = Entropy.Complexity.csv[((10*n.total) + 1):(11*n.total), 1]
  Entropy.Complexity.D5T1$Complexity = Entropy.Complexity.csv[((10*n.total) + 1):(11*n.total), 2]
  Entropy.Complexity.D5T1$Region = regions
  
  trainIndex = createDataPartition(Entropy.Complexity.D5T1$Region, p = split, list = FALSE)
  
  x11 = data.frame(Entropy.Complexity.D5T1$Entropy[trainIndex], Entropy.Complexity.D5T1$Complexity[trainIndex])
  y11 = factor(Entropy.Complexity.D5T1$Region[trainIndex])
  
  x_validation11 = data.frame("Entropy" = Entropy.Complexity.D5T1$Entropy[-trainIndex], "Complexity" = Entropy.Complexity.D5T1$Complexity[-trainIndex])
  y_validation11 = factor(Entropy.Complexity.D5T1$Region[-trainIndex])
  
  Entropy.Complexity.D5T1 = data.frame("Entropy" = Entropy.Complexity.D5T1$Entropy[trainIndex],
                                       "Complexity" = Entropy.Complexity.D5T1$Complexity[trainIndex],
                                       "Region" = Entropy.Complexity.D5T1$Region[trainIndex],
                                       stringsAsFactors=FALSE)
  
  knnFit11 = train(Region~., data = Entropy.Complexity.D5T1, method = "knn",
                   trControl = ctrl,
                   preProcess = c("center","scale"),
                   tuneLength = 20)
  
  pred = predict(knnFit11, newdata = x_validation11)
  
  metrics.calculated$Accuracy[11] = Accuracy(pred, y_validation11)
  metrics.calculated$Recall[11] = Recall(pred, y_validation11)
  metrics.calculated$Precision[11] = Precision(pred, y_validation11)
  metrics.calculated$F1[11] = F1_Score(pred, y_validation11)
  me = ml_test(pred, y_validation11, output.as.table = FALSE)
  metrics.calculated$FPR[11] =  1 - (sum(me$specificity)/3)
  cm = confusionMatrix(table(pred, y_validation11))
  metrics.calculated$sensitivity[11] = sum(diag(cm$table))/sum(cm$table)
  
  #############################################################################################
  ctrl = trainControl(method="repeatedcv", number = 10, repeats = 10, savePredictions = T)
  
  Entropy.Complexity.D5T2 = data.frame("Entropy" = numeric(n.total),
                                       "Complexity" = numeric(n.total),
                                       "Region" = character(n.total),
                                       stringsAsFactors=FALSE)
  
  Entropy.Complexity.D5T2$Entropy = Entropy.Complexity.csv[((11*n.total) + 1):(12*n.total), 1]
  Entropy.Complexity.D5T2$Complexity = Entropy.Complexity.csv[((11*n.total) + 1):(12*n.total), 2]
  Entropy.Complexity.D5T2$Region = regions
  
  trainIndex = createDataPartition(Entropy.Complexity.D5T2$Region, p = split, list = FALSE)
  
  x12 = data.frame(Entropy.Complexity.D5T2$Entropy[trainIndex], Entropy.Complexity.D5T2$Complexity[trainIndex])
  y12 = factor(Entropy.Complexity.D5T2$Region[trainIndex])
  
  x_validation12 = data.frame("Entropy" = Entropy.Complexity.D5T2$Entropy[-trainIndex], "Complexity" = Entropy.Complexity.D5T2$Complexity[-trainIndex])
  y_validation12 = factor(Entropy.Complexity.D5T2$Region[-trainIndex])
  
  Entropy.Complexity.D5T2 = data.frame("Entropy" = Entropy.Complexity.D5T2$Entropy[trainIndex],
                                       "Complexity" = Entropy.Complexity.D5T2$Complexity[trainIndex],
                                       "Region" = Entropy.Complexity.D5T2$Region[trainIndex],
                                       stringsAsFactors=FALSE)
  
  knnFit12 = train(Region~., data = Entropy.Complexity.D5T2, method = "knn",
                   trControl = ctrl,
                   preProcess = c("center","scale"),
                   tuneLength = 20)
  
  pred = predict(knnFit12, newdata = x_validation12)
  
  metrics.calculated$Accuracy[12] = Accuracy(pred, y_validation12)
  metrics.calculated$Recall[12] = Recall(pred, y_validation12)
  metrics.calculated$Precision[12] = Precision(pred, y_validation12)
  metrics.calculated$F1[12] = F1_Score(pred, y_validation12)
  me = ml_test(pred, y_validation12, output.as.table = FALSE)
  metrics.calculated$FPR[12] =  1 - (sum(me$specificity)/3)
  cm = confusionMatrix(table(pred, y_validation12))
  metrics.calculated$sensitivity[12] = sum(diag(cm$table))/sum(cm$table)
  
  #############################################################################################
  ctrl = trainControl(method="repeatedcv", number = 10, repeats = 10, savePredictions = T)
  
  Entropy.Complexity.D5T3 = data.frame("Entropy" = numeric(n.total),
                                       "Complexity" = numeric(n.total),
                                       "Region" = character(n.total),
                                       stringsAsFactors=FALSE)
  
  Entropy.Complexity.D5T3$Entropy = Entropy.Complexity.csv[((12*n.total) + 1):(13*n.total), 1]
  Entropy.Complexity.D5T3$Complexity = Entropy.Complexity.csv[((12*n.total) + 1):(13*n.total), 2]
  Entropy.Complexity.D5T3$Region = regions
  
  trainIndex = createDataPartition(Entropy.Complexity.D5T3$Region, p = split, list = FALSE)
  
  x13 = data.frame(Entropy.Complexity.D5T3$Entropy[trainIndex], Entropy.Complexity.D5T3$Complexity[trainIndex])
  y13 = factor(Entropy.Complexity.D5T3$Region[trainIndex])
  
  x_validation13 = data.frame("Entropy" = Entropy.Complexity.D5T3$Entropy[-trainIndex], "Complexity" = Entropy.Complexity.D5T3$Complexity[-trainIndex])
  y_validation13 = factor(Entropy.Complexity.D5T3$Region[-trainIndex])
  
  Entropy.Complexity.D5T3 = data.frame("Entropy" = Entropy.Complexity.D5T3$Entropy[trainIndex],
                                       "Complexity" = Entropy.Complexity.D5T3$Complexity[trainIndex],
                                       "Region" = Entropy.Complexity.D5T3$Region[trainIndex],
                                       stringsAsFactors=FALSE)
  
  knnFit13 = train(Region~., data = Entropy.Complexity.D5T3, method = "knn",
                   trControl = ctrl,
                   preProcess = c("center","scale"),
                   tuneLength = 20)
  
  pred = predict(knnFit13, newdata = x_validation13)
  
  metrics.calculated$Accuracy[13] = Accuracy(pred, y_validation13)
  metrics.calculated$Recall[13] = Recall(pred, y_validation13)
  metrics.calculated$Precision[13] = Precision(pred, y_validation13)
  metrics.calculated$F1[13] = F1_Score(pred, y_validation13)
  me = ml_test(pred, y_validation13, output.as.table = FALSE)
  metrics.calculated$FPR[13] =  1 - (sum(me$specificity)/3)
  cm = confusionMatrix(table(pred, y_validation13))
  metrics.calculated$sensitivity[13] = sum(diag(cm$table))/sum(cm$table)
  
  #############################################################################################
  ctrl = trainControl(method="repeatedcv", number = 10, repeats = 10, savePredictions = T)
  
  Entropy.Complexity.D5T4 = data.frame("Entropy" = numeric(n.total),
                                       "Complexity" = numeric(n.total),
                                       "Region" = character(n.total),
                                       stringsAsFactors=FALSE)
  
  Entropy.Complexity.D5T4$Entropy = Entropy.Complexity.csv[((13*n.total) + 1):(14*n.total), 1]
  Entropy.Complexity.D5T4$Complexity = Entropy.Complexity.csv[((13*n.total) + 1):(14*n.total), 2]
  Entropy.Complexity.D5T4$Region = regions
  
  trainIndex = createDataPartition(Entropy.Complexity.D5T4$Region, p = split, list = FALSE)
  
  x14 = data.frame(Entropy.Complexity.D5T4$Entropy[trainIndex], Entropy.Complexity.D5T4$Complexity[trainIndex])
  y14 = factor(Entropy.Complexity.D5T4$Region[trainIndex])
  
  x_validation14 = data.frame("Entropy" = Entropy.Complexity.D5T4$Entropy[-trainIndex], "Complexity" = Entropy.Complexity.D5T4$Complexity[-trainIndex])
  y_validation14 = factor(Entropy.Complexity.D5T4$Region[-trainIndex])
  
  Entropy.Complexity.D5T4 = data.frame("Entropy" = Entropy.Complexity.D5T4$Entropy[trainIndex],
                                       "Complexity" = Entropy.Complexity.D5T4$Complexity[trainIndex],
                                       "Region" = Entropy.Complexity.D5T4$Region[trainIndex],
                                       stringsAsFactors=FALSE)
  
  knnFit14 = train(Region~., data = Entropy.Complexity.D5T4, method = "knn",
                   trControl = ctrl,
                   preProcess = c("center","scale"),
                   tuneLength = 20)
  
  pred = predict(knnFit14, newdata = x_validation14)
  
  metrics.calculated$Accuracy[14] = Accuracy(pred, y_validation14)
  metrics.calculated$Recall[14] = Recall(pred, y_validation14)
  metrics.calculated$Precision[14] = Precision(pred, y_validation14)
  metrics.calculated$F1[14] = F1_Score(pred, y_validation14)
  me = ml_test(pred, y_validation14, output.as.table = FALSE)
  metrics.calculated$FPR[14] =  1 - (sum(me$specificity)/3)
  cm = confusionMatrix(table(pred, y_validation14))
  metrics.calculated$sensitivity[14] = sum(diag(cm$table))/sum(cm$table)
  
  #############################################################################################
  ctrl = trainControl(method="repeatedcv", number = 10, repeats = 10, savePredictions = T)
  
  Entropy.Complexity.D5T5 = data.frame("Entropy" = numeric(n.total),
                                       "Complexity" = numeric(n.total),
                                       "Region" = character(n.total),
                                       stringsAsFactors=FALSE)
  
  Entropy.Complexity.D5T5$Entropy = Entropy.Complexity.csv[((14*n.total) + 1):(15*n.total), 1]
  Entropy.Complexity.D5T5$Complexity = Entropy.Complexity.csv[((14*n.total) + 1):(15*n.total), 2]
  Entropy.Complexity.D5T5$Region = regions
  
  trainIndex = createDataPartition(Entropy.Complexity.D5T5$Region, p = split, list = FALSE)
  
  x15 = data.frame(Entropy.Complexity.D5T5$Entropy[trainIndex], Entropy.Complexity.D5T5$Complexity[trainIndex])
  y15 = factor(Entropy.Complexity.D5T5$Region[trainIndex])
  
  x_validation15 = data.frame("Entropy" = Entropy.Complexity.D5T5$Entropy[-trainIndex], "Complexity" = Entropy.Complexity.D5T5$Complexity[-trainIndex])
  y_validation15 = factor(Entropy.Complexity.D5T5$Region[-trainIndex])
  
  Entropy.Complexity.D5T5 = data.frame("Entropy" = Entropy.Complexity.D5T5$Entropy[trainIndex],
                                       "Complexity" = Entropy.Complexity.D5T5$Complexity[trainIndex],
                                       "Region" = Entropy.Complexity.D5T5$Region[trainIndex],
                                       stringsAsFactors=FALSE)
  
  knnFit15 = train(Region~., data = Entropy.Complexity.D5T5, method = "knn",
                   trControl = ctrl,
                   preProcess = c("center","scale"),
                   tuneLength = 20)
  
  pred = predict(knnFit15, newdata = x_validation15)
  
  metrics.calculated$Accuracy[15] = Accuracy(pred, y_validation15)
  metrics.calculated$Recall[15] = Recall(pred, y_validation15)
  metrics.calculated$Precision[15] = Precision(pred, y_validation15)
  metrics.calculated$F1[15] = F1_Score(pred, y_validation15)
  me = ml_test(pred, y_validation15, output.as.table = FALSE)
  metrics.calculated$FPR[15] =  1 - (sum(me$specificity)/3)
  cm = confusionMatrix(table(pred, y_validation15))
  metrics.calculated$sensitivity[15] = sum(diag(cm$table))/sum(cm$table)
  
  #############################################################################################
  ctrl = trainControl(method="repeatedcv", number = 10, repeats = 10, savePredictions = T)
  
  Entropy.Complexity.D6T1 = data.frame("Entropy" = numeric(n.total),
                                       "Complexity" = numeric(n.total),
                                       "Region" = character(n.total),
                                       stringsAsFactors=FALSE)
  
  Entropy.Complexity.D6T1$Entropy = Entropy.Complexity.csv[((15*n.total) + 1):(16*n.total), 1]
  Entropy.Complexity.D6T1$Complexity = Entropy.Complexity.csv[((15*n.total) + 1):(16*n.total), 2]
  Entropy.Complexity.D6T1$Region = regions
  
  trainIndex = createDataPartition(Entropy.Complexity.D6T1$Region, p = split, list = FALSE)
  
  x16 = data.frame(Entropy.Complexity.D6T1$Entropy[trainIndex], Entropy.Complexity.D6T1$Complexity[trainIndex])
  y16 = factor(Entropy.Complexity.D6T1$Region[trainIndex])
  
  x_validation16 = data.frame("Entropy" = Entropy.Complexity.D6T1$Entropy[-trainIndex], "Complexity" = Entropy.Complexity.D6T1$Complexity[-trainIndex])
  y_validation16 = factor(Entropy.Complexity.D6T1$Region[-trainIndex])
  
  Entropy.Complexity.D6T1 = data.frame("Entropy" = Entropy.Complexity.D6T1$Entropy[trainIndex],
                                       "Complexity" = Entropy.Complexity.D6T1$Complexity[trainIndex],
                                       "Region" = Entropy.Complexity.D6T1$Region[trainIndex],
                                       stringsAsFactors=FALSE)
  
  knnFit16 = train(Region~., data = Entropy.Complexity.D6T1, method = "knn",
                   trControl = ctrl,
                   preProcess = c("center","scale"),
                   tuneLength = 20)
  
  pred = predict(knnFit16, newdata = x_validation16)
  
  metrics.calculated$Accuracy[16] = Accuracy(pred, y_validation16)
  metrics.calculated$Recall[16] = Recall(pred, y_validation16)
  metrics.calculated$Precision[16] = Precision(pred, y_validation16)
  metrics.calculated$F1[16] = F1_Score(pred, y_validation16)
  me = ml_test(pred, y_validation16, output.as.table = FALSE)
  metrics.calculated$FPR[16] =  1 - (sum(me$specificity)/3)
  cm = confusionMatrix(table(pred, y_validation16))
  metrics.calculated$sensitivity[16] = sum(diag(cm$table))/sum(cm$table)
  
  #############################################################################################
  ctrl = trainControl(method="repeatedcv", number = 10, repeats = 10, savePredictions = T)
  
  Entropy.Complexity.D6T2 = data.frame("Entropy" = numeric(n.total),
                                       "Complexity" = numeric(n.total),
                                       "Region" = character(n.total),
                                       stringsAsFactors=FALSE)
  
  Entropy.Complexity.D6T2$Entropy = Entropy.Complexity.csv[((16*n.total) + 1):(17*n.total), 1]
  Entropy.Complexity.D6T2$Complexity = Entropy.Complexity.csv[((16*n.total) + 1):(17*n.total), 2]
  Entropy.Complexity.D6T2$Region = regions
  
  trainIndex = createDataPartition(Entropy.Complexity.D6T2$Region, p = split, list = FALSE)
  
  x17 = data.frame(Entropy.Complexity.D6T2$Entropy[trainIndex], Entropy.Complexity.D6T2$Complexity[trainIndex])
  y17 = factor(Entropy.Complexity.D6T2$Region[trainIndex])
  
  x_validation17 = data.frame("Entropy" = Entropy.Complexity.D6T2$Entropy[-trainIndex], "Complexity" = Entropy.Complexity.D6T2$Complexity[-trainIndex])
  y_validation17 = factor(Entropy.Complexity.D6T2$Region[-trainIndex])
  
  Entropy.Complexity.D6T2 = data.frame("Entropy" = Entropy.Complexity.D6T2$Entropy[trainIndex],
                                       "Complexity" = Entropy.Complexity.D6T2$Complexity[trainIndex],
                                       "Region" = Entropy.Complexity.D6T2$Region[trainIndex],
                                       stringsAsFactors=FALSE)
  
  knnFit17 = train(Region~., data = Entropy.Complexity.D6T2, method = "knn",
                   trControl = ctrl,
                   preProcess = c("center","scale"),
                   tuneLength = 20)
  
  pred = predict(knnFit17, newdata = x_validation17)
  
  metrics.calculated$Accuracy[17] = Accuracy(pred, y_validation17)
  metrics.calculated$Recall[17] = Recall(pred, y_validation17)
  metrics.calculated$Precision[17] = Precision(pred, y_validation17)
  metrics.calculated$F1[17] = F1_Score(pred, y_validation17)
  me = ml_test(pred, y_validation17, output.as.table = FALSE)
  metrics.calculated$FPR[17] =  1 - (sum(me$specificity)/3)
  cm = confusionMatrix(table(pred, y_validation17))
  metrics.calculated$sensitivity[17] = sum(diag(cm$table))/sum(cm$table)
  
  #############################################################################################
  ctrl = trainControl(method="repeatedcv", number = 10, repeats = 10, savePredictions = T)
  
  Entropy.Complexity.D6T3 = data.frame("Entropy" = numeric(n.total),
                                       "Complexity" = numeric(n.total),
                                       "Region" = character(n.total),
                                       stringsAsFactors=FALSE)
  
  Entropy.Complexity.D6T3$Entropy = Entropy.Complexity.csv[((17*n.total) + 1):(18*n.total), 1]
  Entropy.Complexity.D6T3$Complexity = Entropy.Complexity.csv[((17*n.total) + 1):(18*n.total), 2]
  Entropy.Complexity.D6T3$Region = regions
  
  trainIndex = createDataPartition(Entropy.Complexity.D6T3$Region, p = split, list = FALSE)
  
  x18 = data.frame(Entropy.Complexity.D6T3$Entropy[trainIndex], Entropy.Complexity.D6T3$Complexity[trainIndex])
  y18 = factor(Entropy.Complexity.D6T3$Region[trainIndex])
  
  x_validation18 = data.frame("Entropy" = Entropy.Complexity.D6T3$Entropy[-trainIndex], "Complexity" = Entropy.Complexity.D6T3$Complexity[-trainIndex])
  y_validation18 = factor(Entropy.Complexity.D6T3$Region[-trainIndex])
  
  Entropy.Complexity.D6T3 = data.frame("Entropy" = Entropy.Complexity.D6T3$Entropy[trainIndex],
                                       "Complexity" = Entropy.Complexity.D6T3$Complexity[trainIndex],
                                       "Region" = Entropy.Complexity.D6T3$Region[trainIndex],
                                       stringsAsFactors=FALSE)
  
  knnFit18 = train(Region~., data = Entropy.Complexity.D6T3, method = "knn",
                   trControl = ctrl,
                   preProcess = c("center","scale"),
                   tuneLength = 20)
  
  pred = predict(knnFit18, newdata = x_validation18)
  
  metrics.calculated$Accuracy[18] = Accuracy(pred, y_validation18)
  metrics.calculated$Recall[18] = Recall(pred, y_validation18)
  metrics.calculated$Precision[18] = Precision(pred, y_validation18)
  metrics.calculated$F1[18] = F1_Score(pred, y_validation18)
  me = ml_test(pred, y_validation18, output.as.table = FALSE)
  metrics.calculated$FPR[18] =  1 - (sum(me$specificity)/3)
  cm = confusionMatrix(table(pred, y_validation18))
  metrics.calculated$sensitivity[18] = sum(diag(cm$table))/sum(cm$table)
  
  #############################################################################################
  ctrl = trainControl(method="repeatedcv", number = 10, repeats = 10, savePredictions = T)
  
  Entropy.Complexity.D6T4 = data.frame("Entropy" = numeric(n.total),
                                       "Complexity" = numeric(n.total),
                                       "Region" = character(n.total),
                                       stringsAsFactors=FALSE)
  
  Entropy.Complexity.D6T4$Entropy = Entropy.Complexity.csv[((18*n.total) + 1):(19*n.total), 1]
  Entropy.Complexity.D6T4$Complexity = Entropy.Complexity.csv[((18*n.total) + 1):(19*n.total), 2]
  Entropy.Complexity.D6T4$Region = regions
  
  trainIndex = createDataPartition(Entropy.Complexity.D6T4$Region, p = split, list = FALSE)
  
  x19 = data.frame(Entropy.Complexity.D6T4$Entropy[trainIndex], Entropy.Complexity.D6T4$Complexity[trainIndex])
  y19 = factor(Entropy.Complexity.D6T4$Region[trainIndex])
  
  x_validation19 = data.frame("Entropy" = Entropy.Complexity.D6T4$Entropy[-trainIndex], "Complexity" = Entropy.Complexity.D6T4$Complexity[-trainIndex])
  y_validation19 = factor(Entropy.Complexity.D6T4$Region[-trainIndex])
  
  Entropy.Complexity.D6T4 = data.frame("Entropy" = Entropy.Complexity.D6T4$Entropy[trainIndex],
                                       "Complexity" = Entropy.Complexity.D6T4$Complexity[trainIndex],
                                       "Region" = Entropy.Complexity.D6T4$Region[trainIndex],
                                       stringsAsFactors=FALSE)
  
  knnFit19 = train(Region~., data = Entropy.Complexity.D6T4, method = "knn",
                   trControl = ctrl,
                   preProcess = c("center","scale"),
                   tuneLength = 20)
  
  pred = predict(knnFit19, newdata = x_validation19)
  
  metrics.calculated$Accuracy[19] = Accuracy(pred, y_validation19)
  metrics.calculated$Recall[19] = Recall(pred, y_validation19)
  metrics.calculated$Precision[19] = Precision(pred, y_validation19)
  metrics.calculated$F1[19] = F1_Score(pred, y_validation19)
  me = ml_test(pred, y_validation19, output.as.table = FALSE)
  metrics.calculated$FPR[19] =  1 - (sum(me$specificity)/3)
  cm = confusionMatrix(table(pred, y_validation19))
  metrics.calculated$sensitivity[19] = sum(diag(cm$table))/sum(cm$table)
  
  #############################################################################################
  ctrl = trainControl(method="repeatedcv", number = 10, repeats = 10, savePredictions = T)
  
  Entropy.Complexity.D6T5 = data.frame("Entropy" = numeric(n.total),
                                       "Complexity" = numeric(n.total),
                                       "Region" = character(n.total),
                                       stringsAsFactors=FALSE)
  
  Entropy.Complexity.D6T5$Entropy = Entropy.Complexity.csv[((19*n.total) + 1):(20*n.total), 1]
  Entropy.Complexity.D6T5$Complexity = Entropy.Complexity.csv[((19*n.total) + 1):(20*n.total), 2]
  Entropy.Complexity.D6T5$Region = regions
  
  trainIndex = createDataPartition(Entropy.Complexity.D6T5$Region, p = split, list = FALSE)
  
  x20 = data.frame(Entropy.Complexity.D6T5$Entropy[trainIndex], Entropy.Complexity.D6T5$Complexity[trainIndex])
  y20 = factor(Entropy.Complexity.D6T5$Region[trainIndex])
  
  x_validation20 = data.frame("Entropy" = Entropy.Complexity.D6T5$Entropy[-trainIndex], "Complexity" = Entropy.Complexity.D6T5$Complexity[-trainIndex])
  y_validation20 = factor(Entropy.Complexity.D6T5$Region[-trainIndex])
  
  Entropy.Complexity.D6T5 = data.frame("Entropy" = Entropy.Complexity.D6T5$Entropy[trainIndex],
                                       "Complexity" = Entropy.Complexity.D6T5$Complexity[trainIndex],
                                       "Region" = Entropy.Complexity.D6T5$Region[trainIndex],
                                       stringsAsFactors=FALSE)
  
  knnFit20 = train(Region~., data = Entropy.Complexity.D6T5, method = "knn",
                   trControl = ctrl,
                   preProcess = c("center","scale"),
                   tuneLength = 20)
  
  pred = predict(knnFit20, newdata = x_validation20)
  
  metrics.calculated$Accuracy[20] = Accuracy(pred, y_validation20)
  metrics.calculated$Recall[20] = Recall(pred, y_validation20)
  metrics.calculated$Precision[20] = Precision(pred, y_validation20)
  metrics.calculated$F1[20] = F1_Score(pred, y_validation20)
  me = ml_test(pred, y_validation20, output.as.table = FALSE)
  metrics.calculated$FPR[20] =  1 - (sum(me$specificity)/3)
  cm = confusionMatrix(table(pred, y_validation20))
  metrics.calculated$sensitivity[20] = sum(diag(cm$table))/sum(cm$table)
  
}


plot.dimension.tau <- function(){

  D = c(rep(3,5), rep(4,5), rep(5,5), rep(6,5))
  point.shape = c(rep(15,5), rep(16,5), rep(17,5), rep(18,5))
  line.shape = c(rep(2,5), rep(3,5), rep(4,5), rep(5,5))
  tau = rep(c(1,2,3,4,5), 4)
  
  D = factor(D)
  
  
  metrics.calculated.csv = read.csv("../Data/metricsCalculated.csv")

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

     pdf("ROC.pdf", width = 6, height = 4)
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
     print(ROC.curve)
     dev.off()
}

plot.dimension.tau()
