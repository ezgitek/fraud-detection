library(DMwR)
library(caret) 
library(data.table)

options(scipen = 999) #for the numbers not to appear in scientific notation

startTime <- Sys.time()

#clean up data
data[data=='Y'] <- 1
data[data=='N'] <- 0
data <- data[complete.cases(data),]
depCount = ncol(data)
data[, 1:(depCount-1)] <- lapply(data[,1:(depCount-1)], as.numeric)
data <- data[complete.cases(data),]

#randomly divide the data into 70% train set and 30% test set
splitIndex <- createDataPartition(data$FRAUD, p = .70, list = FALSE, times = 1)
trainData <- data[splitIndex]
testData <- data[-splitIndex]

#oversample and try
trainData$FRAUD <- factor(trainData$FRAUD, labels=c(0,1))
trainData <- SMOTE(FRAUD ~ ., trainData, perc.over = 100, perc.under = 200)
trainData$FRAUD <- as.numeric(as.character(trainData$FRAUD))

$cross validation, 5-fold
ctrl <- trainControl(method = "cv", number = 5)

#random forest
if(trainMethod == "rf"){
	model <- train(FRAUD ~ ., data = trainData, method = trainMethod, trControl = ctrl, importance = TRUE)
}else{ #treebag or partitioning
	model <- train(FRAUD ~ ., data = trainData, method = trainMethod, trControl = ctrl)
}

#predict
predictorVariables <- names(trainData)[names(trainData) != 'FRAUD']
pred <- predict(model, testData[, predictorVariables])
