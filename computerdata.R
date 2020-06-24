# Predict Price of the computer
# 
# A dataframe containing :
#   
#   price : price in US dollars of 486 PCs
# 
# speed : clock speed in MHz
# 
# hd : size of hard drive in MB
# 
# ram : size of Ram in in MB
# 
# screen : size of screen in inches
# 
# cd : is a CD-ROM present ?
#   
#   multi : is a multimedia kit (speakers, sound card) included ?
#   
#   premium : is the manufacturer was a "premium" firm (IBM, COMPAQ) ?
#   
#   ads : number of 486 price listings for each month
# 
# trend : time trend indicating month starting from January of 1993 to November of 1995.

setwd("H:\\RStudio\\assignment\\")
computerdat <-read.csv(file.choose())
getwd()
View(computerdat)
attach(computerdat)
computerdat["cd1"]<-factor(computerdat$cd,levels = c("yes","no"),labels = c(1,0))
computerdat["multi1"]<-factor(computerdat$multi,level=c("yes","no"),label=c(1,0))
computerdat["premium1"]<-factor(computerdat$premium,level=c("yes","no"),label=c(1,0))
cd <-computerdat[,-c(1,7,8,9)]
View(cd)
attach(cd)
str(cd)
class(cd)
cor(cd[sapply(cd,is.numeric)],use="pairwise")
colnames(cd)
summary(cd)
pairs(cd)

library(car)
###first model
cdata1 <-lm(price~.,data=cd)
###summary
summary(cdata1)
##invoking MASS package.
library(MASS)
###AIC
stepAIC(cdata1)
##Multilinearity
car::vif(cdata1)
##plot
plot(cdata1)
###residuals vs regressor
residualPlots(cdata1)
###qqplot
qqplot(cdata1)
###avPlots
avPlots(cdata1)
###influenceIndexPlot
influenceIndexPlot(cdata1)
influencePlot(cdata1)


###Predict first model
pred1cd <-predict(cdata1,cd)

###correlation between predi1 and price
cor(pred1cd,cd$price)

###root mean square error
rmse1cd <-mean(cdata1$residuals^2)^.5
rmse1cd

cda1 <-cd[-c(1441,1701,3784,4478),]
cdata2 <-lm(price~.,data=cda1)
summary(cdata2)
stepAIC(cdata2)
plot(cdata2)
qqplot(cdata2)

vif(cdata2)
influenceIndexPlot(cdata2)
influencePlot(cdata2)
pred2cd <-predict(cdata2,cd)
cor(pred2cd,cd$price)
rmse2cd <-mean(cdata2$residuals^2)^.5
rmse2cd

cda2 <-cd[-c(1441,1701,3784,4478,1689,5961,4478,3704,994),]
cdata3 <-lm(price~.,data=cda2)
summary(cdata3)
stepAIC(cdata3)
plot(cdata3)
qqplot(cdata3)

vif(cdata3)
influenceIndexPlot(cdata3)
influencePlot(cdata3)
pred3cd <-predict(cdata3,cd)
cor(pred3cd,cd$price)
rmse3cd <-mean(cdata3$residuals^2)^.5
rmse3cd


cda3 <-cd[-c(1441,1701,3784,4478,1689,5961,4478,3704,994,1049,1785),]
cdata4 <-lm(price~.,data=cda3)
summary(cdata4)
stepAIC(cdata4)
plot(cdata4)
qqplot(cdata4)

vif(cdata4)
influenceIndexPlot(cdata4)
influencePlot(cdata4)
pred4cd <-predict(cdata4,cd)
cor(pred4cd,cd$price)
rmse4cd <-mean(cdata4$residuals^2)^.5
rmse4cd


cda4 <-cd[-c(1441,1701,3784,4478,1689,5961,4478,3704,994,1049,1785,1102,721),]
cdata5 <-lm(price~.,data=cda4)
summary(cdata5)
stepAIC(cdata5)
plot(cdata5)
qqplot(cdata5)

vif(cdata5)
influenceIndexPlot(cdata5)
influencePlot(cdata5)
pred4cd <-predict(cdata5,cd)
cor(pred4cd,cd$price)
rmse4cd <-mean(cdata5$residuals^2)^.5
rmse4cd

cda5 <-cd[-c(1441,1701,3784,4478,1689,5961,4478,3704,994,1049,1785,1102,721,901,161),]
cdata6 <-lm(price~.,data=cda5)
summary(cdata6)
stepAIC(cdata6)
plot(cdata6)
qqplot(cdata6)

vif(cdata6)
influenceIndexPlot(cdata6)
influencePlot(cdata6)
pred5cd <-predict(cdata6,cd)
cor(pred5cd,cd$price)
rmse5cd <-mean(cdata6$residuals^2)^.5
rmse5cd
