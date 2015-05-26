source("aode.R")
source("utils.R")

################ Walidacja poprawności ################  

data <- read.csv("data/weather.csv", header=TRUE, sep=",")

res <- testAODE(formula = play~.,
                trainingData = data,
                testData = data,
                aodeM = 1 )

print(res)
toTexTable(res)

################ Porównanie weather ################  

data <- read.csv("data/weather.csv", header=TRUE, sep=",")

res <- doTestForAllAlgorithms(
  formula = play~.,
  trainingData = data,
  testData= data,
  aodeM = 1 )

print(res)

toTexTable(res)

################ Car ################  
# liczba klas: 4

data <- read.csv("data/car.data", header = TRUE, sep=",")

splData = splitData(data, seed = 1234)

formula = eval~.

res <- doTestForAllAlgorithms(
  formula = formula,
  trainingData = splData$trainset,
  testData= splData$testset,
  aodeM = 1 )

print(res)

toTexTable(res)

################ connect-4 ################  
# liczba atrybutów: 42

data <- read.csv("data/connect-4.data", sep=",")
splData = splitData(data, traningDataLen = 2000, testDataLen = 1000)

colnames(data)[length(colnames(data))] <- "class"
formula = class~.

system.time({
  
  res <- doTestForAllAlgorithms(
    formula = formula,
    trainingData = data,
    testData= data,
    aodeM = 1 )
})

toTexTable(res)

###############