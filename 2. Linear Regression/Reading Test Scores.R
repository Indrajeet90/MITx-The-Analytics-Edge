# ~~~~~~~ Reading Test Scores ~~~~~~~~~~~~~~~~
# Loading the datasets
pisaTrain <- read.csv("pisa2009train.csv")
pisaTest <- read.csv("pisa2009test.csv")
str(pisaTrain)

# Summarizing the dataset
tapply(pisaTrain$readingScore, pisaTrain$male,mean) # Average reading score of male/female

# Locating missing values
summary(pisaTrain)

# Removing missing values
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)

str(pisaTrain)# 2414 obs.
str(pisaTest) # 990 obs.

# Building a regression model using factor variables
?relevel
# Releveling the factors
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

lmScore = lm(readingScore ~.,data = pisaTrain)
summary(lmScore)
# The training-set RMSE can be computed by first computing the SSE, then taking the square root
SSE = sum(lmScore$residuals^2)
RMSE = sqrt(SSE / nrow(pisaTrain))
sqrt(mean(lmScore$residuals^2)) # alternative way

# Predicting the outcome
predTest = predict(lmScore, newdata = pisaTest)
summary(predTest)

SSE= sum((pisaTest$readingScore - predTest)^2)
SSE     # 5762082 for this model
RMSE = sqrt(SSE/nrow(pisaTest))
RMSE    # 76.29079 for the model

# Baseline prediction and test-set SSE
baseline = mean(pisaTrain$readingScore)
baseline 
SST = sum((baseline-pisaTest$readingScore)^2) # Total sum of squares
SST

# Test-set R-squared
R2 = 1- (SSE/SST)
R2
