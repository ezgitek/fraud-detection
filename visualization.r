library(ggplot2)
library(pROC)
library(corrplot)

#area under roc curve as a performance measure
auc <- roc(testData$FRAUD, pred)

threshold <- coords(auc, "best", ret = "threshold")
totalTime <- Sys.time() - startTime


outFile = file(paste(filePath, 'analysis/', trainMethod, '.txt', sep=''))

#variable importance
imp <- varImp(model, scale=FALSE)
sink(outFile)

print(paste('Time elapsed', totalTime, sep=' '))
print(paste('Data count', nrow(data), sep=' '))
print(paste('Train model', trainMethod, sep=' '))
writeLines('')

print('Fraud and not fraud percentages in sample')
print(prop.table(table(data$FRAUD)))
print(caret::confusionMatrix(testData$FRAUD, factor(as.numeric(pred > threshold)), 
	positive = levels(testData$FRAUD[2], dnn = c("Prediction", "Actual")))

sink()
close(outFile)

#correlation plot
png(paste(filePath, "analysis/corrplot.png", sep=""), width=900, height=600)
print(corrplot(cor(data[, 1:depCount-1]), method = "circle"))

dev.off()

#ROC 
png(paste(filePath, "analysis/",trainMethod,"_plot.png", sep=""), width=900, height=600)
plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('Area Under Curve: ', round(auc$auc[[1]], 2)))
abline(h=1, col='blue',lwd=2)
abline(h=0, col='red',lwd=2)

dev.off()

#example feature qplot for two features
png(paste(filePath, "analysis/",trainMethod,"_f1_f2_qplot.png", sep=""), width=900, height=600)
data$F1 <- as.factor(data$F1)
data$F2 <- as.factor(data$F2)

print(ggplot(data, aes(x=F1, y=F2, color=FRAUD, shape=FRAUD)) + geom_point(size=4, alpha=0.6))

dev.off()







