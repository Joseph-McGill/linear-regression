## Joseph McGill
## Spring 2016
## Simple linear regression project using hockey data from the 2014-2015
## NHL season

## Full file path to the hockey_full.csv file
dird=""

#Function to concatenate file name to dird, then read the data
myread=function(csv){
  fl=paste(dird,csv,sep="")
  read.table(fl,header=TRUE,sep=",")
}

#Read the data into R
hockey.df = myread("hockey_full.csv")

#Make a trend scatter plot to see the trend in the data
library(s20x)
trendscatter(W~GA, data = hockey.df, f = 0.7)

#Make a linear model for the data
hockey.lm = lm(W~GA, data = hockey.df)

#Check for normality in the residuals
normcheck(hockey.lm, shapiro.wilk = TRUE)

#Plot residuals vs fitted values of the linear model
plot(hockey.lm, which = 1)

#Summary of the linear model
summary(hockey.lm)

#Get the estimates for the parameters
hockey.lm$coefficients

#Confidence interval for B0 and B1
ciReg(hockey.lm)

#Make predictions using the linear model
predict(hockey.lm, data.frame(GA=c(185, 200, 215)))

#Plot the predictions with the actual data
with(hockey.df,plot(W~GA,bg="Black",main="Hockey Linear Model",
    pch=22,cex=1.2,ylim=c(20,1.1*max(W)),xlim=c(170,1.1*max(GA))))
abline(hockey.lm,col='Red',lwd=2)
predictions = c(185, 200, 215)

points(predictions, predict(hockey.lm, data.frame(GA=c(185, 200, 215))),
       col='Blue',cex=1.6,pch=19)

#Cooks distance plot
cooks20x(hockey.lm)
