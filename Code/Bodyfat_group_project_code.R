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
library(timeDate)
#setwd("") set the directory to any you want
#/Users/shravankaul/Downloads/BodyFat.csv
BodyFat = read.csv("BodyFat.csv") #Read data into R


# Impute height of the 42th individual:
BodyFat[42,6] <- sqrt(BodyFat[42,5]*703/BodyFat[42,7])

# Impute Bodyfat of the 182nd individual: 
BodyFat[182,2] <- 7.3
BodyFat <- subset( BodyFat, select = c(-1,-3) )
head(BodyFat) 
summary(BodyFat) #Brief Summary Statistics
plot_histogram(BodyFat)
plot_boxplot(BodyFat, by="BODYFAT")

# Removing Outliers
BodyFat_t<-BodyFat
for(col in colnames(BodyFat)){
  Q <- quantile(BodyFat[,col], probs=c(.25, .75), na.rm = FALSE)
  iqr <- IQR(BodyFat[,col])
  up <-  Q[2]+3*iqr # Upper Range  
  low <- Q[1]-3*iqr # Lower Range
  if(col=="BODYFAT"){low<- 0.5}
  BodyFat_t <- BodyFat_t %>% filter(.data[[col]]<up, .data[[col]]>low) 
}
#Outliers removed
BodyFat_o<-anti_join(BodyFat,BodyFat_t)
#Max or Min ofAnkle,Knee, Height,ANKLE ,BODYFAT


plot(BodyFat_t$AGE,BodyFat_t$BODYFAT)

# correlation matrix heatmap
cormat <- round(cor(BodyFat_t),2)  #Correlation matrix can be created using the R function cor() :
melted_cormat <- melt(cormat)  #melt the correlation matrix
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "black")+
  #The function geom_tile() in ggplot2 package is used to visualize the correlation matrix 
  scale_fill_gradient2(low = "green", high = "red", mid = "yellow", 
                       midpoint = 0, limit = c(-1,1), space = "Lab") + geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)  
kurtosis(BodyFat)

# Selection of the significant predictors using general linear model
summary(model1 <- lm(BODYFAT ~  ABDOMEN+AGE+WRIST    , data = BodyFat_t))

summary(model2 <- lm(BODYFAT ~ ABDOMEN + HEIGHT + WRIST , data = BodyFat_t))
summary(all <- lm(BODYFAT ~ ., data=BodyFat_t))
intercept_only <- lm(BODYFAT ~ 1, data=BodyFat_t)
summary(model3 <- lm(BODYFAT ~  ABDOMEN+FOREARM+WRIST    , data = BodyFat_t))
summary(model4 <- lm(BODYFAT ~  ABDOMEN+WEIGHT+WRIST    , data = BodyFat_t))
summary(model4additive <- lm(BODYFAT ~  ABDOMEN*WEIGHT+WRIST    , data = BodyFat_t))
both <- step(intercept_only, direction='forward', scope=formula(all),criterion=c("BIC"))
both$anova
summary(both)
#test train split
set.seed(123)
training.samples <- BodyFat_t$BODYFAT %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- BodyFat_t[training.samples, ]
test.data <- BodyFat_t[-training.samples, ]


predictions <- model4 %>% predict(test.data)

# Model performance
# (a) Prediction error, RMSE
RMSE(predictions, test.data$BODYFAT)
# (b) R-square
R2(predictions, test.data$BODYFAT)


BIC(model1)
BIC(model2)
BIC(model3)
BIC(model4)
BIC(model4additive)
# Repeated K fold Validation
train_control <- trainControl(method = "repeatedcv",
                              number = 5,repeats = 5)


model <- train(BODYFAT ~  ABDOMEN+WEIGHT+WRIST , data = BodyFat_t, 
               method = "lm",
               trControl = train_control)
print(model)

dat<- BodyFat_t
#Final model plot
plot.lm.final = ggplot(dat, aes(x= (ABDOMEN*0.88)+(WEIGHT*-0.08)+(WRIST*-1.36)-22.94, y=BODYFAT)) + 
  geom_point() +
  stat_smooth(method='lm', formula = y ~ x, size = 1) + 
  xlab('Predictors') + ylab('Bodyfat percentage')

print(plot.lm.final)



#Linearity
plot(predict(model4),resid(model4),pch=19,cex=0.6,cex.lab=1.5,cex.main=1.5,
     xlab="Predicted Body Fat %", ylab="Standardized Residuals",main="Standardized Residual Plot")
abline(a=0,b=0,col="black",lwd=2)

#Normality
qqnorm(rstandard(model4),pch=19,cex=0.6,cex.lab=1.5,cex.main=1.5,
       main="Normal Q-Q Plot of the Residuals")
abline(a=0,b=1,col="black",lwd=2)

#Leverage and Influential Points
pii = hatvalues(model4)
cooki = cooks.distance(model4)
n = dim(BodyFat_t)[1]
plot(1:n,pii,type="p",pch=19,cex=0.5,cex.lab=1.5,cex.main=1.5,
     xlab="Index (Each Observation)",ylab="Pii",main="Leverage Values (Pii)")
plot(1:n,cooki,type="p",pch=19,cex=0.5,cex.lab=1.5,cex.main=1.5,
     xlab="Index (Each Observation)",ylab="Cook's Distance",main="Influence Values (Cook's Distance)")




# Prediction Interval for an example
ABDOMEN <- 85
WEIGHT <- 155
WRIST <- 18
df <- data.frame(ABDOMEN,WEIGHT,WRIST)
predict(model4, df, interval="predict",level=0.5) 








