library(readxl)
library(plyr)
library(mvinfluence)
library(corpcor)
computer <- read.csv("F:/ExcelR/excelRASS/ass10 Mutilinear reg/computer/Computer_Data.csv")
computer <- computer[-1]
View(computer)
str(computer)
computer$cd <- as.factor(revalue(computer$cd,c("yes"=1, "no"=0)))
computer$multi <- as.factor(revalue(computer$multi,c("yes"=1, "no"=0)))
computer$premium <- as.factor(revalue(computer$premium,c("yes"=1, "no"=0)))
str(computer)
attach(computer)
#checking how each vairables are associated with each other (corelation).
plot(speed,price)
plot(hd,price)
plot(ram,price)
plot(screen,price)
plot(cd,price)
plot(multi,price)
plot(premium,price)
plot(ads,price)
plot(trend,price)
pairs(computer)
summary(computer)
#converting factors into int to perform cor
computer<- cbind(price,speed,hd,ram,screen,cd,multi,premium,ads,trend)
computer <- as.data.frame(computer)
attach(computer)
str(computer)
#checking the colinearity between the variables.
cor(computer)
model_Computer= lm(price~speed+hd+ram+screen+cd+multi+premium+ads+trend, data = computer)
summary(model_Computer) #from this we came to know that all the variables are statistically signficant to the model 
                        #so we cannot remove any variables
#finding out the variables that influence and deleting them
influence.measures(model_Computer)
influencePlot(model_Computer) #1441,1701 most influencing
#Logarithmic transformations and deleting influencing data.
model_ComputerLog= lm(price~log(speed)+log(hd)+log(ram)+log(screen)+log(cd)+log(multi)+log(premium)+log(ads)+log(trend), data = computer[-c(1441,1701),])
AIC(model_ComputerLog)
summary(model_ComputerLog) #rsquared 0.7441 AIC value = 88840.6
vif(model_ComputerLog)
#Exponential transformation
model_ComputerExpo= lm(log(price)~speed+hd+ram+screen+cd+multi+premium+ads+trend, data = computer[-c(1441,1701),])
AIC(model_ComputerExpo)
summary(model_ComputerExpo) #rsquared 0.7833 AIC value = -8773.967
vif(model_ComputerExpo)
#Quadratic transformation
model_ComputerQuad= lm(price~speed+I(speed^2)+hd+I(hd^2)+ram+I(ram^2)+screen+I(screen^2)+cd+I(cd^2)+multi+I(multi^2)+premium+I(premium^2)+ads+I(ads^2)+trend+I(trend^2), data = computer[-c(1441,1701),])
AIC(model_ComputerQuad)
summary(model_ComputerQuad) #rsquared 0.8049 AIC value = 86889.96
#vif(model_ComputerQuad) has pure multicolinearity
#polynomial regression transfromation
model_ComputerPoly= lm(price~speed+I(speed^2)+I(speed^3)+hd+I(hd^2)+I(hd^3)+ram+I(ram^2)+I(ram^3)+screen+I(screen^2)+I(screen^3)+cd+I(cd^2)+I(cd^3)+multi+I(multi^2)+I(multi^3)+premium+I(premium^2)+I(premium^3)+ads+I(ads^2)+I(ads^3)+trend+I(trend^2)+I(trend^3), data = computer[-c(1441,1701),])
summary(model_ComputerPoly) #rsquared 0.813 AIC value = 86889.96
AIC(model_ComputerPoly)
#alias(model_ComputerPoly)
#vif(model_ComputerPoly) has pure multicolinearity
#here the exponential model has less AIC value which means less information loss so we take that as final model
final_model<-model_ComputerExpo
summary(final_model)
confint(final_model)
predictprice<-predict(final_model,interval = "predict")
predictprice
final<-cbind(computer$price,computer$speed,computer$hd,computer$ram,computer$screen,computer$cd,computer$multi,computer$premium,predictprice)
View(final)
plot(final_model)
