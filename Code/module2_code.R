library(psych)
library(magrittr)
library(RcmdrMisc)
library(dplyr)
library(ggplot2)
library(reshape2)
library(car)
library(DataExplorer)
library(randomForest)
library(datasets)
library(tidyverse)
theme_set(theme_classic())
library(caret)
library(flexmix)

#setwd to set directory set("*****")
BodyFat <- read.csv("BodyFat.csv")

summary(BodyFat) 
## Correlation matrix heatmap
cormat <- round(cor(BodyFat),2)  #Correlation matrix can be created using the R function cor() :
melted_cormat <- melt(cormat)  #melt the correlation matrix
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "black")+
  #The function geom_tile() in ggplot2 package is used to visualize the correlation matrix 
  scale_fill_gradient2(low = "green", high = "red", mid = "yellow", 
                       midpoint = 0, limit = c(-1,1), space = "Lab") + geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)  

## Outlier detection:
BodyFat_t <- BodyFat
for(col in colnames(BodyFat[,c(-1)])){
  Q <- quantile(BodyFat[,col], probs=c(.25, .75), na.rm = FALSE)
  iqr <- IQR(BodyFat[,col])
  up <-  Q[2]+3*iqr # Upper Range  
  low <- Q[1]-3*iqr # Lower Range
  if(col=="BODYFAT"){low<- 0.5}
  BodyFat_t <- BodyFat_t %>% filter(.data[[col]]<up, .data[[col]]>low) 
}
BodyFat_o <- anti_join(BodyFat, BodyFat_t)

## Detection for individuals that may need to be imputed:
# (a) Adiposity vs weight & height
calculated_bmi <- BodyFat$WEIGHT/(BodyFat$HEIGHT)^2*703
d <- calculated_bmi-BodyFat$ADIPOSITY
sus <- which(abs(d)>0.5) #42
cbind(BodyFat[sus,], d[sus])
# Three individuals are find, but we only impute the height of 42 since it is really 
# abnormal for a man with height of 29.50 inches.

# (b) Bodyfat vs density
cal_fat <- 495/BodyFat$DENSITY-450
fat_diff <- cal_fat - BodyFat$BODYFAT 
sus2 <- which(abs(fat_diff)>2)
cbind(BodyFat[sus2,], cal_fat[sus2]) # 182
# Bodyfat of 182 is 0 and its calculated bodyfat is negative, -3.61, which is even impossible. 
# Therefore, we impute bodyfat of 182 by using bodyfat calculator on www.calculator.net.

# According to the results of outlier detection, we decided to impute 42 and 182, and remove the other three points.
# Impute height of the 42th individual:

BodyFat[42,6] <- round(sqrt(BodyFat[42,5]*703/BodyFat[42,7]), digits = 2)
# Impute Bodyfat of the 182nd individual: 
BodyFat[182,2] <- 7.3
# Remove outliers
BodyFat <- BodyFat[-c(31, 39, 86),] 

# CLeaned data
data_new <- BodyFat[,-c(1,3)]
# write.csv(data_new, file = "~/Desktop/STAT 628/Body Fat/CleanedData.csv")

## Model comparison:
# Stepwise variable selection using BIC criterion: forward, backword and both direction
full_model <- lm(BODYFAT ~ ., data = data_new)
step_model <- stepwise(full_model, direction = "forward", criterion = c("BIC"), trace = FALSE) 
summary(step_model) # Final model

step_model1 <- stepwise(full_model, direction = "backward", criterion = c("BIC"), trace = FALSE) 
summary(step_model1)

step_model2 <- stepwise(full_model, direction = "backward/forward", criterion = c("BIC"), trace = FALSE) 
summary(step_model2) 

# Stepwise variable selection using AIC criterion:  
step_model_aic <- stepwise(full_model, direction = "forward", criterion = c("AIC"), trace = FALSE) 
summary(step_model_aic) # Removing nonsignificant predictor BICEPS

step_model_aic1 <- stepwise(full_model, direction = "backward", criterion = c("AIC"), trace = FALSE) 
summary(step_model_aic1)

step_model_aic2 <- stepwise(full_model, direction = "backward/forward", criterion = c("AIC"), trace = FALSE) 
summary(step_model_aic2)

## Check for interaction terms:
summary(model4additive <- lm(BODYFAT ~ ABDOMEN*WEIGHT + WRIST, data = data_new))
summary(model4additive1 <- lm(BODYFAT ~ ABDOMEN + WEIGHT*WRIST, data = data_new))

## Final model:
summary(model4 <- lm(BODYFAT ~ ABDOMEN + WEIGHT + WRIST, data = data_new)) 


## Final model plot
plot.lm.final = ggplot(BodyFat_t, aes(x= (ABDOMEN*0.88)+(WEIGHT*-0.08)+(WRIST*-1.36)-22.94, y=BODYFAT)) + 
  geom_point() +
  stat_smooth(method='lm', formula = y ~ x, size = 1) + 
  xlab('Predictors') + ylab('Bodyfat percentage')

print(plot.lm.final)


## Model Diagnostic:
# Linearity
plot(predict(model4),resid(model4),pch=19,cex=0.6,cex.lab=1.5,cex.main=1.5,
     xlab="Predicted Body Fat %", ylab="Standardized Residuals",main="Standardized Residual Plot")
abline(a=0,b=0,col="black",lwd=2)

# Normality
qqnorm(rstandard(model4),pch=19,cex=0.6,cex.lab=1.5,cex.main=1.5,
       main="Normal Q-Q Plot of the Residuals")
abline(a=0,b=1,col="black",lwd=2)


## Trial in report 3.Model part
newdata1=BodyFat[1,]
newdata1$ABDOMEN[1]=85
newdata1$WEIGHT=155
newdata1$WRIST=18
predict(model4, newdata1, interval="predict",level=0.5) 
