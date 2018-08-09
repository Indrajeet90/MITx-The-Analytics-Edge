# ~~~~~~~  Detecting Flu Epidemics via Search Engine Query Data ~~~~~~~~~~

FluTrain = read.csv("FluTrain.csv")
FluTest = read.csv("FluTest.csv")
str(FluTrain)

# Understanding the Data
which.max(FluTrain$ILI)  #303
FluTrain$Week[303]
FluTrain$Week[which.max(FluTrain$Queries)]

hist(FluTrain$ILI) # visualizing distribution of ILI
plot(FluTrain$Queries,log(FluTrain$ILI), main="Log of ILI versus Queries", ylab="Log of ILI", xlab="Queries", col="dark red")

# ~~~~ Linear Regression Model~~~~~~~~
FluTrend1 = lm( log(ILI)~ Queries, data = FluTrain)
summary(FluTrend1)
cor(FluTrain$ILI, FluTrain$Queries)  #  0.8142115 for this dataset

# ~~~~~~~~~ Performance on the Test Set ~~~~~~~~~~
predic1 = exp(predict(FluTrend1, newdata = FluTest))
summary(predic1)

#To determine which element in the test set is for March 11, 2012
which(FluTest$Week == "2012-03-11 - 2012-03-17") 
predic1[11]

# Performance on the Test Set
Relerr = ( FluTest$ILI[11]- predic1[11]) /FluTest$ILI[11] # (Observed ILI - Estimated ILI)/Observed ILI
Relerr
RMSE = sqrt(sum(FluTest$ILI-predic1)^2/nrow(FluTest))
RMSE
# RMSE= sqrt(mean((PredTest1-FluTest$ILI)^2)) can also be used

# ~~~~~~~~ Training a Time Series Model ~~~~~~~~~~~~~~~~~~~~~~~~
library(zoo)
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
# value of -2 passed to lag means to return 2 observations before the current one; a positive value would have returned future observations. 
# The parameter na.pad=TRUE means to add missing values for the first two weeks of our dataset 
 
FluTrain$ILILag2 = coredata(ILILag2)
summary(FluTrain$ILILag2)

plot(log(FluTrain$ILILag2),log(FluTrain$ILI), main=" Plot of ILI v Lag ")

# Trainig a time-series model
FluTrend2 = lm(log(ILI) ~ Queries + log(ILILag2), data = FluTrain)
summary(FluTrend2)

# Evaluating the Time Series Model in the Test Set
ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)
summary(FluTest$ILILag2)

# Filling in the missing data
FluTest$ILILag2[1] = FluTrain$ILI[416]
FluTest$ILILag2[1]
FluTest$ILILag2[2] = FluTrain$ILI[417]
FluTest$ILILag2[2]

# Predicting ILI 
PredTest2  = exp(predict(FluTrend2, newdata = FluTest))
RMSE = sqrt((mean(FluTest$ILI- PredTest2)^2))
RMSE