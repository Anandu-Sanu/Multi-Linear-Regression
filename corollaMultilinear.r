library(readxl)
library(plyr)
library(mvinfluence)
library(corpcor)
corolla <- read.csv("F:/ExcelR/excelRASS/ass10 Mutilinear reg/corolla./Toyotacorolla.csv")
corolla<- corolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
View(corolla)
str(corolla)
summary(corolla)
#checking how each vairables are associated with each other (corelation).
plot(corolla$Age_08_04,corolla$Price)
plot(corolla$KM,corolla$Price)
plot(corolla$HP,corolla$Price)
plot(corolla$cc,corolla$Price)
plot(corolla$Doors,corolla$Price)
plot(corolla$Gears,corolla$Price)
plot(corolla$Quarterly_Tax,corolla$Price)
plot(corolla$Weight,corolla$Price)
pairs(corolla)
summary(corolla)
#checking the colinearity between the variables.
cor(corolla)
model_corolla= lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight, data = corolla)
summary(model_corolla) #from this we came to know that cc and doors provides less statistically signficant  
                       #information to the model so we can remove them.
model_corolla1= lm(Price~Age_08_04+KM+HP+Gears+Quarterly_Tax+Weight, data = corolla)
summary(model_corolla1)
#finding out the variables that influence and deleting them
influence.measures(model_corolla1)
influencePlot(model_corolla1) 
vif(model_corolla1)
avPlots(model_corolla1)

#here the exponential model has less AIC value which means less information loss so we take that as final model
final_model<-model_corolla1
summary(final_model)
confint(final_model)
predictprice<-predict(final_model,interval = "predict")
predictprice
final<-cbind(corolla$Price,corolla$Age_08_04,corolla$KM,corolla$HP,corolla$cc,corolla$Doors,corolla$Gears,corolla$Quarterly_Tax,corolla$Weight,predictprice)
View(final)
plot(final_model)
