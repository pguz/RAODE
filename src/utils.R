library("e1071")
library("RWeka")
library("bnlearn")
source("aode.R")

doTestForAllAlgorithms <- function(formula, trainingData, testData, aodeM)
{
  result <- testAODE(formula, trainingData, testData, aodeM)
  row.names(result)[1] <- "AODE"
  
  result <- rbind(result, testNaiveBayes(formula, trainingData, testData))
  row.names(result)[2] <- "Naive Bayes"
  
  result <- rbind(result, testC45(formula, trainingData, testData))
  row.names(result)[3] <- "C4.5"
  
  result <- rbind(result, testTAN(formula, trainingData, testData))
  row.names(result)[4] <- "TAN"
    
  result
}

testAODE <- function(formula, trainingData, testData, aodeM)
{
  aodew <- aode(formula, trainingData)
  pred <- prediction.aode(aodew, aodeM, testData)
  win <- cbind(pred, apply(pred, 1, function(x) noquote(names(which.max(x)))))[,3]
  win <- sapply(win, noquote)
  
  
  result <- calcRatesFor(formula, testData, win) 
  result
}

testNaiveBayes <-function(formula, trainingData, testData)
{
  model <- naiveBayes(formula, trainingData)
  pred <- predict(model, testData)
  
  result <- calcRatesFor(formula, testData, pred ) 
  result
}

testC45 <-function(formula, trainingData, testData, cost = NULL, numFolds = 0, complexity = FALSE, class = FALSE, seed = NULL)
{
  model <- J48(formula, data = trainingData)

  e <- evaluate_Weka_classifier(model,
                                cost = cost, 
                                numFolds = numFolds, 
                                complexity = complexity , 
                                class = class, 
                                seed = seed)
  
  result <- calcRates(e$confusionMatrix ) 
  result
}

testTAN <- function(formula, trainingData, testData)
{
  cl <- toString(formula[[2]])
  model <- tree.bayes(trainingData, cl)
  
  pred <- predict(model, testData)
  
  result <- calcRatesFor(formula, testData, pred ) 
  result
}

calcRatesFor <- function(formula, dataForEvaluation, result)
{
  cl <- all.vars(formula[[2]])
  
  predicted <- sapply(result, function(x) noquote(toString(x)))
  trues = dataForEvaluation[[cl]]
  
  rates <- calcRatesFromData(predicted, trues)
  rates
}

calcRatesFromData <- function(predicted, trues){
 
  xTab <- table(predicted, trues)
  calcRates(xTab)
}

calcRates <- function(xTab){

  r <- matrix(NA, ncol = 6, nrow = 1, 
              dimnames = list(c(),c('Accuracy',
                                    'Error',
                                    'Recall/Sensivity',
                                    'Precision',
                                    'Specifity',
                                    'FMeasure')))
  TN <- xTab[1,1]
  FN <- xTab[1,2]
  FP <- xTab[2,1]
  TP <- xTab[2,2]
  
  r[1,1] <- (TN + TP)/sum(xTab) # Accuracy
  r[1,2] <- (FN + FP)/sum(xTab) # Error
  
  TPRate = TP / sum( FN + TP )
  FPRate = FP / sum( TN + FP )
  
  r[1,3] <- TPRate  # Recall / Sensivity
  r[1,4] <- TP / (FP + TP) # Precision
  r[1,5] <- 1 - FPRate # Specifity
  r[1,6] <- 2 / ( 1/r[1,3] + 1/r[1,4]) # FMeasure
  
  r
}


toTexTable <- function(data, filename, addRowName = FALSE)
{
  fileConn <- file(filename)
  if(addRowName)
  {
    linie = c("\\hline", paste(" & ", paste(colnames(data), collapse = " & ")))  
  }
  else
  {
    linie = c("\\hline", paste(colnames(data), collapse = " & "))
  }
  
  for(i in 1:(length(data[,1])))
  {
    linie = append(linie, "\\\\ \\hline")
    if(addRowName)
    {
      newLine = paste(c(row.names(data)[i], sapply(data[i,], toString)), collapse = " & ")
    }
    else
    {
      newLine = paste(sapply(data[i,], toString), collapse = " & ")
    }
    linie = append(linie, newLine)      
  }
  linie = append(linie,"\\\\ \\hline")
  writeLines(linie, fileConn)
  
  close(fileConn)
}
