# Prepare a prediction model for profit of 50_startups data.
# Do transformations for getting better predictions of profit and
# make a table containing R^2 value for each prepared model.
# 
# R&D Spend -- Research and devolop spend in the past few years
# Administration -- spend on administration in the past few years
# Marketing Spend -- spend on Marketing in the past few years
# State -- states from which data is collected
# Profit  -- profit of each state in the past few years

setwd("H:\\RStudio\\assignment\\")
startups <-read.csv(file.choose())
attach(startups)
View(startups)
str(startups)
head(startups)

start <-startups[c("R.D.Spend","Administration","Marketing.Spend","Profit")]
View(start)
cor(start[sapply(start,is.numeric)],use="pairwise")
plot(start)
library(car)
library(MASS)
start1_50 <-lm(Profit~.,data=start)
summary(start1_50)
stepAIC(start1_50)
##Multi-colinearity..
car::vif(start1_50)

#plot
plot(start1_50)

##residuals vs regressor
residualPlots(start1_50)
##qqplot
qqplot(start1_50)
##avPort
avPlots(start1_50)
##influenceIndex
influenceIndexPlot(start1_50)
influencePlot(start1_50)
#predict

pred1 <-predict(start1_50,start)
cor(pred1,start$Profit)
rmse1 <-mean(start1_50$residuals^2)^.5
rmse1
start2_50 <- lm(Profit~R.D.Spend+Administration+Marketing.Spend, data = start[-c(50,49),])

summary(start2_50)


vif(start2_50)

stepAIC(start2_50)
plot(start2_50)
qqplot(start2_50)
residualPlots(start2_50)
avPlots(start2_50)
influenceIndexPlot(start2_50)
influencePlot(start2_50)
pred2 <-predict(start2_50,start)
cor(pred2,start$Profit)
rmse2 <-mean(start2_50$residuals^2)^.5
rmse2

start3_50 <-lm(Profit~R.D.Spend+Marketing.Spend,data=start[-c(47,15)])
summary(start3_50)
vif(start3_50)
stepAIC(start3_50)
plot(start3_50)
qqplot(start3_50)
residualPlots(start3_50)
avPlots(start3_50)
influenceIndexPlot(start3_50)
influencePlot(start3_50)
pred3 <-predict(start3_50,start)
cor(pred3,start$Profit)
rmse3 <-mean(start3_50$residuals^2)^.5
rmse3
