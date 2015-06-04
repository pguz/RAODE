source("aode.R")

data <- read.csv("../data/weather.csv", header=TRUE, sep=",")

aodew <- aode(play~., data)
result <- prediction.aode(aodew, 1, data)
cl <- all.vars((play~.)[[2]])
win <- cbind(round(result, digits = 3), cbind(apply(result, 1, function(x) noquote(names(which.max(x))))), data[cl])
colnames(win)[3] <- "predict"
colnames(win)[4] <- "real"
print(win)
