source("aode.R")
source("utils.R")

################ Walidacja poprawności ################  

data <- read.csv("weather.csv", header=TRUE, sep=",")

res <- testAODE(formula = play~.,
                trainingData = data,
                testData= data,
                aodeM = 1 )

print(res)

################ Porównanie weather ################  

data <- read.csv("weather.csv", header=TRUE, sep=",")


res <- doTestForAllAlgorithms(
                formula = play~.,
                trainingData = data,
                testData= data,
                aodeM = 1 )

print(res)
