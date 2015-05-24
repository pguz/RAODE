source("aode.R")

data <- read.csv("weather.csv", header=TRUE, sep=",")

aodew <- aode(play~., data)
result <- prediction.aode(aodew, 1, data)
win <- cbind(result, apply(result, 1, function(x) noquote(names(which.max(x)))))

print(win)