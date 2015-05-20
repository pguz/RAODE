setwd("/media/kedrzu/Dane/studia/MOW/Projekt/RAODE/src/")


source("utils.R")

data <- read.csv("weather.csv", header=TRUE, sep=",")

formula = play~.
trainingData = data
testData= data
aodeM = 1

res <- doTestForAllAlgorithms(formula = play~., trainingData = data, testData= data, aodeM = 1 )
res