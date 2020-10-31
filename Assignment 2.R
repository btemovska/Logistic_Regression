install.packages("ISLR")
library(ISLR)

#1 load the data and display the top 5
data<-Smarket
head(data)
#  Year   Lag1   Lag2   Lag3   Lag4   Lag5 Volume  Today Direction
#1 2001  0.381 -0.192 -2.624 -1.055  5.010 1.1913  0.959        Up
#2 2001  0.959  0.381 -0.192 -2.624 -1.055 1.2965  1.032        Up
#3 2001  1.032  0.959  0.381 -0.192 -2.624 1.4112 -0.623      Down
#4 2001 -0.623  1.032  0.959  0.381 -0.192 1.2760  0.614        Up
#5 2001  0.614 -0.623  1.032  0.959  0.381 1.2057  0.213        Up
#6 2001  0.213  0.614 -0.623  1.032  0.959 1.3491  1.392        Up

#2 Check for missing values
complete.cases(data) #all TRUE
sum(is.na(data)) #[1] 0
#no missing values

#3 convert the last column into numerical values
data$Direction <- ifelse(data$Direction == "Up", 1, 0)
data$Direction <- factor(data$Direction, levels = c(0, 1))
for(i in 1:10) {
  data[, i] <- as.numeric(as.character(data[, i]))
}
head(data)
#  Year   Lag1   Lag2   Lag3   Lag4   Lag5 Volume  Today Direction
#1 2001  0.381 -0.192 -2.624 -1.055  5.010 1.1913  0.959         1
#2 2001  0.959  0.381 -0.192 -2.624 -1.055 1.2965  1.032         1
#3 2001  1.032  0.959  0.381 -0.192 -2.624 1.4112 -0.623         0
#4 2001 -0.623  1.032  0.959  0.381 -0.192 1.2760  0.614         1
#5 2001  0.614 -0.623  1.032  0.959  0.381 1.2057  0.213         1
#6 2001  0.213  0.614 -0.623  1.032  0.959 1.3491  1.392         1

#4set seed to 100 and split (training & test set)
set.seed(100)
library(caret)
trainDataIndex <- createDataPartition(data$Direction, p=0.7, list = F)
trainData <-data[trainDataIndex,] #875 obs
testData<-data[-trainDataIndex, ] #375 obs

table(trainData$Direction)
# 0   1 
# 418 457 
table(testData$Direction)
#  0   1 
# 184 191 

#5 Fit LOGISTIC REGRESSION
logitmod <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume + Today, data = trainData)
summary(logitmod)

#Call:
# glm(formula = Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + 
#       Volume + Today, data = trainData)
# 
# Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
# -1.26408  -0.31379   0.01791   0.31776   1.09825  
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.524635   0.049244  10.654   <2e-16 ***
#   Lag1         0.006463   0.010350   0.624   0.5325    
# Lag2        -0.002545   0.010460  -0.243   0.8079    
# Lag3         0.017948   0.010455   1.717   0.0864 .  
# Lag4         0.004166   0.010507   0.397   0.6918    
# Lag5         0.004917   0.010551   0.466   0.6413    
# Volume      -0.006878   0.032453  -0.212   0.8322    
# Today        0.321408   0.010210  31.478   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for gaussian family taken to be 0.1168304)
# 
# Null deviance: 218.32  on 874  degrees of freedom
# Residual deviance: 101.29  on 867  degrees of freedom
# AIC: 614.45
# 
# Number of Fisher Scoring iterations: 2

#6 predict on the test set
pred <- predict(logitmod, newdata = testData, type = "response")
y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0,1))
y_act <- testData$Direction
mean(y_pred == y_act) #0.9733333  We achive 97% accuracy on test using logistic regression
