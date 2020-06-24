library(readxl)
library(ggplot2)
#toyota<-read_excel(file.choose())
toyota <- read.csv("H:\\RStudio\\assignment\\assignments1\\MultiLinear-Regression\\ToyotaCorolla.csv")
colSums(is.na(toy))
View(toyota)
toy <-toyota[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
summary(toy)
str(toy)
cor(toy[sapply(toy, is.numeric)],use='pairwise')

View(toy)
install.packages("car")
library(car)
#Scatter Plot Matrix:
pairs(toy)
attach(toy)
#Correlation Matrix
#cor(t)
?cor
#Regression Model and Summary
toy1.car<-lm(Price~.,data = toy)
summary(toy1.car)
library(MASS)
stepAIC(toy1.car)



#Multi-colinearity

#Multi-colinearity
#Variance Inflation Factor
car::vif(toy1.car)

#Diagnostic Plots:
#Residual Plots, QQ-Plos, Std. Residuals vs Fitted
plot(toy1.car) 
#Residuals vs Regressors
residualPlots(toy1.car)
#Added Variable Plots
avPlots(toy1.car)
#QQ plots of studentized residuals
qqPlot(toy1.car)
#Deletion Diagnostics
influenceIndexPlot(toy1.car) # Index Plots of the influence measures

####Iteration 1 
#Remove 77th observation
toy["Doors1"]<-toy$Doors*toy$Doors
toy["Gears1"]<-toy$Gears*toy$Gears

toy1<-toy[-c(81,222),]
model1<-lm(Price~.,data =toy1)
summary(model1)

plot(model1) 
residualPlots(model1)
qqPlot(model1)
influenceIndexPlot(model1)

vif(model1)
#iteration2
toy2<-toy[-c(222,961,81,602,600,959),]

model2<-lm(Price~.,data = toy2[,-c(6,8)])
summary(model2)
plot(model2) 
residualPlots(model2)
qqPlot(model2)
influenceIndexPlot(model2)
#predict(model2)
toy3 <-toy[-c(222,961,81,602,600,959,148,655,147,651),]
model3 <-lm(Price~.,data=toy3[,-c(6,8)])
summary(model3)
# plot(model3)
residualPlots(model3)
qqplot(model3)
influenceIndexPlot(model3)
 vif(model3)
# 
toy4 <-toy[-c(222,961,81,602,600,959,148,655,147,651,992,957,524,193),]
model4 <-lm(Price~.,data=toy4[,-c(6,8)])
summary(model4)
plot(model4)
residualPlots(model4)
influenceIndexPlot(model4)
vif(model4)
qqplot(model4)
# 
toy5 <-toy[-c(222,961,81,602,600,959,148,655,147,651,992,957,524,193,110,192,172),]
model5 <-lm(Price~.,data=toy5)
summary(model5)
plot(model5)
residualPlots(model5)
influenceIndexPlot(model5)
vif(model5)


toy6 <-toy[-c(222,961,81,602,600,959,148,655,147,651,992,957,524,193,110,192,172,1079,1059,394),]
model6 <-lm(Price~.,data=toy6)
summary(model6)
plot(model6)
residualPlots(model6)
influenceIndexPlot(model6)
qqplot(model6)
vif(model6)

toy7 <-toy[-c(222,961,81,602,600,959,148,655,147,651,992,957,524,193,110,192,172,1079,1059,394,403,190,126),]
model7 <-lm(Price~.,data=toy7)
summary(model7)
plot(model7)
residualPlots(model7)
influenceIndexPlot(model7)
vif(model7)

toy8 <-toy[-c(222,961,81,602,600,959,148,655,147,651,992,957,524,193,110,192,172,1079,1059,394,403,190,126,1436,1352,17),]
model8 <-lm(Price~.,data=toy8)
summary(model8)
plot(model8)
residualPlots(model8)
influenceIndexPlot(model8)
vif(model8)


toy9 <-toy[-c(222,961,81,602,600,959,148,655,147,651,992,957,524,193,110,192,172,1079,1059,394,403,190,126,1436,1352,17,142,762,75),]
model9 <-lm(Price~.,data=toy9)
summary(model9)
plot(model9)
stepAIC(model9)
residualPlots(model9)
influenceIndexPlot(model9)
vif(model9)