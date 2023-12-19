#SMargalef
#Moneyball in the NBA
#Overview

#This repository explores the concept of Moneyball in the NBA using R. 
#Two datasets, NBA1980_2011 and NBA2013, are utilized. The analysis aims 
#to predict the number of wins needed to make the playoffs and the points 
#needed to achieve this goal. The prediction is then compared to the 2012-2013 season dataset.


#Load the data
NBA = read.csv("NBA1980_2011.csv")
summary(NBA)

#Table to see how many wins to make the playoffs
table(NBA$W, NBA$Playoffs)
#We can say that with at least 42 wins, you will make the playoffs

#Calculate the points difference
NBA$PTSdiff = NBA$PTS - NBA$oppPTS

#Check for linear relationship and create a linear regression model for wins
plot(NBA$PTSdiff, NBA$W)
WinsModel = lm(W ~ PTSdiff, data=NBA)
summary(WinsModel)
#We can say that for every point difference, you will get 0.032 wins
#The R^2 value is 0.94, which is a good fit

#PTSdiff >= (42-41)/0.0326 = 30.67

#Create a linear regression model for points scored
PointsModel = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL, data=NBA)
summary(PointsModel)
#R^2 value is 0.89

#Sum of Squared Errors
sum(PointsModel$residuals^2)
#Result = 28421465

#Root mean squared error
sqrt(sum(PointsModel$residuals^2)/nrow(NBA))
#Result = 184.493

#Read the 2012-13 NBA data set
NBA2013 = read.csv("NBA2013.csv")

#Make predictions on test set
PointsPredictions = predict(PointsModel, newdata=NBA2013)

#Compute out of sample R^2
SSE = sum((PointsPredictions - NBA2013$PTS)^2)
SST = sum((mean(NBA2013$PTS) - NBA2013$PTS)^2)
R2 = 1 - SSE/SST
R2

#Compute the RMSE
sqrt(SSE/nrow(NBA2013))
