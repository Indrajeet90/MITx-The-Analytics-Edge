# ~~~~~~~~ Climate Change ~~~~~~~~~~~~~~
library(readr)

climate_change <- read.csv("D:/Learning/MIT Courses/The Analytics Edge/Unit2/Unit2/climate_change.csv")
str(climate_change)

train = subset(climate_change, Year <= 2006)
test = subset(climate_change,Year > 2006)
str(train)
str(test)

# Creating regression model
model = lm(Temp ~ MEI +CO2+CH4+N2O+CFC.11+CFC.12+TSI+Aerosols, data = train)
summary(model)
# Residual standard error: 0.09171 on 275 degrees of freedom
# Multiple R-squared:  0.7509,	Adjusted R-squared:  0.7436 
# F-statistic: 103.6 on 8 and 275 DF,  p-value: < 2.2e-16
cor(train)

# Modifying the regression model
model1 = lm(Temp ~ MEI +N2O+TSI+Aerosols, data = train)
summary(model1)
#Residual standard error: 0.09547 on 279 degrees of freedom
#Multiple R-squared:  0.7261,	Adjusted R-squared:  0.7222 
#F-statistic: 184.9 on 4 and 279 DF,  p-value: < 2.2e-16

# Automatically Building the Model
automodel = step(model)
summary(automodel)

#Residual standard error: 0.09155 on 276 degrees of freedom
#Multiple R-squared:  0.7508,	Adjusted R-squared:  0.7445 
#F-statistic: 118.8 on 7 and 276 DF,  p-value: < 2.2e-16

model2 = lm(Temp ~ MEI +CO2+N2O+CFC.11+CFC.12+TSI+Aerosols, data = train)
summary(model2)

# Testing on Unseen Data
predic = predict(automodel, newdata = test)
SSE = sum((predic - test$Temp)^2)
SST = sum((mean(train$Temp) - test$Temp)^2)
R2 = 1-(SSE/SST)
R2