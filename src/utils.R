library("e1071")
library("RWeka")
library("bnlearn")

message("Do RWeka trzeba doinstalowac pakiet lazyBayesianRules przy użyciu komendy: WPM(\"install-package\", \"lazyBayesianRules\")")

########################################################################################

doTestForAllAlgorithms <- function(formula, trainingData, testData, aodeM)
{
  idx = 1
  #print(system.time(replicate(2, testAODE(formula, trainingData, testData, aodeM))))
  result <- testAODE(formula, trainingData, testData, aodeM)
  row.names(result)[idx] <- "AODE"
  
  idx = idx + 1
  #print(system.time(replicate(2, testNaiveBayes(formula, trainingData, testData))))
  result <- rbind(result, testNaiveBayes(formula, trainingData, testData))
  #result <- testNaiveBayes(formula, trainingData, testData)
  row.names(result)[idx] <- "NB (e1071)"
  
  idx = idx + 1
  #print(system.time(replicate(2, testNB(formula, trainingData, testData))))
  result <- rbind(result, testNB(formula, trainingData, testData))
  row.names(result)[idx] <- "NB (bnlearn)"
  
  idx = idx + 1
  #print(system.time(replicate(2, testTAN(formula, trainingData, testData))))
  result <- rbind(result, testTAN(formula, trainingData, testData))
  row.names(result)[idx] <- "TAN"
  
  idx = idx + 1
  #print(system.time(replicate(2, testLBR(formula, trainingData, testData))))
  #result <- rbind(result, testLBR(formula, trainingData, testData))
  #row.names(result)[idx] <- "LBR"

  #idx = idx + 1
  #print(system.time(replicate(2, testC45(formula, trainingData, testData))))
  result <- rbind(result, testC45(formula, trainingData, testData))
  row.names(result)[idx] <- "C4.5"
  
  #idx = idx + 1
  #print(system.time(replicate(2, testSVM(formula, trainingData, testData))))
  #result <- rbind(result, testSVM(formula, trainingData, testData))
  #row.names(result)[idx] <- "SVM"
  
  result
}

########################################################################################

testAODE <- function(formula, trainingData, testData, aodeM)
{
  aodew <- aode(formula, trainingData)
  testAODEWithModel(formula, aodew, testData, aodeM)
}

testAODEWithModel <- function(formula, model, testData, aodeM)
{
  pred <- prediction.aode(model = model, m = aodeM, data = testData)
  win <- apply(pred, 1, function(x) noquote(names(which.max(x))))
  
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

testNB <-function(formula, trainingData, testData)
{
  cl <- toString(formula[[2]])
  model <- naive.bayes(trainingData, cl)
  pred <- predict(model, testData)
  result <- calcRatesFor(formula, testData, pred ) 
  result
}


testC45 <-function(formula, trainingData, testData)
{
  model <- J48(formula, trainingData)
  
  e <- evaluate_Weka_classifier(model, testData)
  
  result <- calcRates(e$confusionMatrix ) 
  result
}

testTAN <- function(formula, trainingData, testData)
{
  cl <- toString(formula[[2]])
  tan <- tree.bayes(trainingData, cl)
  fitted = bn.fit(tan, trainingData, method = "bayes")
  pred <- predict(fitted, testData)
  
  result <- calcRatesFor(formula, testData, pred ) 
  result
}

testLBR <-function(formula, trainingData, testData, cost = NULL, numFolds = 0, complexity = FALSE, class = FALSE, seed = NULL)
{
  model <- LBR(formula, data = trainingData)
  
  e <- evaluate_Weka_classifier(model,
                                newdata = testData,
                                cost = cost, 
                                numFolds = numFolds, 
                                complexity = complexity , 
                                class = class, 
                                seed = seed)
  
  result <- calcRates(e$confusionMatrix ) 
  result
}

testSVM <-function(formula, trainingData, testData)
{
  
  model   <- svm(formula, data = trainingData)
  pred <- predict(model, testData)
  result <- calcRatesFor(formula, testData, pred) 
  result
}

########################################################################################

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
  
  result = 0
  if(length(xTab[1,]) == 2 && length(xTab[,1]) == 2 )
  {
    result <- matrix(NA, ncol = 6, nrow = 1, 
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
    
    result[1,1] <- (TN + TP)/sum(xTab) # Accuracy
    result[1,2] <- (FN + FP)/sum(xTab) # Error
    
    TPRate = TP / sum( FN + TP )
    FPRate = FP / sum( TN + FP )
    
    result[1,3] <- TPRate  # Recall / Sensivity
    result[1,4] <- TP / (FP + TP) # Precision
    result[1,5] <- 1 - FPRate # Specifity
    result[1,6] <- 2 / ( 1/result[1,3] + 1/result[1,4]) # FMeasure
  } else {
    
    result <- matrix(NA, ncol = 2, nrow = 1, 
                     dimnames = list(c(), c('Accuracy', 
                                            'Error')))
    result[1,1] = sum(diag(xTab)) / sum(xTab)
    result[1,2] = 1 - result[1,1]
  }
  
  result
}

########################################################################################

dyskretyzuj <- function(data, colnum, method = "quantile", breaks = 3)
{
  przedzialy = discretize(data = data, method = "quantile", breaks = breaks)
  data[,colnum] <- przedzialy[,1]
  data
}

########################################################################################

splitData <- function(data, traningDataLen = round(nrow(data)/2), testDataLen = NULL, seed=12349) {
  

  set.seed(seed)
  
  index <- 1:nrow(data)
  trainindex <- sample(index, size = traningDataLen)
  
  trainset <- data[trainindex, ]
  testset <- data[-trainindex, ]
  
  if(!is.null(testDataLen) && nrow(testset) > testDataLen)
  {
    index <- 1:testDataLen
    trainindex <- sample(index, size = testDataLen)
    testset <- testset[trainindex, ]
  } 
  
  list(trainset=trainset,testset=testset)
}

########################################################################################

toTexTable <- function(data, addRowName = TRUE, filename = NULL)
{
  linie = c("\begin{tabular}{ |l|l|l|l|l| }")
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
    rowStr = sapply(data[i,], function(x) toString(round(x, 3)))
    if(addRowName)
    {
      newLine = paste(c(row.names(data)[i], rowStr), collapse = " & ")
    }
    else
    {
      newLine = paste(rowStr, collapse = " & ")
    }
    linie = append(linie, newLine)      
  }
  linie = append(linie,"\\\\ \\hline")
  if(!is.null(filename))
  {
    fileConn <- file(filename)
    writeLines(linie, fileConn)
    close(fileConn)
    
  } else {
    writeLines(linie)
  }
}

########################################################################################

