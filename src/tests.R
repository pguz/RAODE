source("aode.R")
source("utils.R")

#cat("\014") // wyczyść konsolę

################ Walidacja poprawności ################  

data <- read.csv("../data/weather.csv", header=TRUE, sep=",")

res <- testAODE(formula = play~.,
                trainingData = data,
                testData = data,
                aodeM = 1 )

print(res)
toTexTable(res)

################ Porównanie weather ################  

data <- read.csv("../data/weather.csv", header=TRUE, sep=",")

res <- doTestForAllAlgorithms(
  formula = play~. ,
  trainingData = data,
  testData = data,
  aodeM = 1 )

print(res)
toTexTable(res)

################ Car ################  
# liczba klas: 4

data <- read.csv("../data/car.data", header = TRUE, sep=",")

splData = splitData(data, seed = 1234)

formula = eval~.

res <- doTestForAllAlgorithms(
  formula = formula,
  trainingData = splData$trainset,
  testData= splData$testset,
  aodeM = 100 )

print(res)
toTexTable(res)


# tylko dla AODE (testowanie parametru m)
#aodew <- aode(formula, splData$trainset)
#pred <- prediction.aode(model = aodew, m = 280, data = splData$testset)
#win <- apply(pred, 1, function(x) noquote(names(which.max(x))))

#result <- calcRatesFor(formula, splData$testset, win) 
#print(result)

#toTexTable(result)

################ mushroom ################  
# liczba klas: 4

data <- read.csv("../data/mushroom.data", sep=",")
n <- names(data)
toRemove <- c("p.2")

data <- data[, !(n %in% toRemove)]

splData = splitData(data, seed = 1234)

formula = u~.

res <- doTestForAllAlgorithms(
  formula = formula,
  trainingData = splData$trainset,
  testData= splData$testset,
  aodeM = 0 )

print(res)
toTexTable(res)


# tylko dla AODE (testowanie parametru m)
#aodew <- aode(formula, splData$trainset)
#pred <- prediction.aode(model = aodew, m = 3900, data = splData$testset)
#win <- apply(pred, 1, function(x) noquote(names(which.max(x))))

#result <- calcRatesFor(formula, splData$testset, win) 
#print(result)

#toTexTable(result)
