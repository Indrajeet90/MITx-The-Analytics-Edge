pisaTrain <- read_csv("D:/Learning/MIT Courses/The Analytics Edge/Unit2/Unit2/pisa2009train.csv")
View(pisaTrain)
# Tapply to find mean of reading score of male
tapply(pisaTrain$readingScore, pisaTrain$male, mean)

pisaTest <- read_csv("D:/Learning/MIT Courses/The Analytics Edge/Unit2/Unit2/pisa2009test.csv")
# Omitting NA values
pisaTrain = na.omit(pisaTrain)
str(pisaTrain)

# Change raceeth from character to factor
pisaTrain$raceeth=as.factor(pisaTrain$raceeth
pisaTest$raceeth=as.factor(pisaTest$raceeth)

#Set the reference level of the factor
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

# Building a linear regression model
#"readingScore ~ ." to mean "predict readingScore using all the other variables in the data frame." 
lmScore=lm(readingScore~.,data=pisaTrain)
summary(lmScore)
# Finding errors
SSE = sum(lmScore$residuals^2)
RMSE

#Prediction vector using Regression model for new dataset
predTest=predict(lmScore,newdata = pisaTest)
summary(predTest)
