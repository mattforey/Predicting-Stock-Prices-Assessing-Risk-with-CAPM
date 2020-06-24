library(tseries)
library(quantmod)
library(libridate)
library(dplyr)
library(MASS)
library(tree)
library(quantmod)
library(tseries)
library(e1071)

# read data
DowData <- read.delim("C:/Users/matt4/Downloads/dow_jones_index.data", header=TRUE, sep=",")
NamesData <- read.delim("C:/Users/matt4/Downloads/dow_jones_index.names", header=TRUE, sep=",")


# Remove any missing values if needed
DowData = na.omit(DowData)
#went from 750 obs to 720

# See what the data classes are
str(DowData)

DowData <- as.data.frame(DowData)

#DowData$date <- mdy(as.character(DowData$date))

# Change columns 4-7 to numeric values keeping decimals
DowData$open <- as.character(gsub("\\$", "", DowData$open))
DowData$open <- as.numeric(DowData$open, options(digits = 6))
DowData$high <- as.character(gsub("\\$", "", DowData$high))
DowData$high <- as.numeric(DowData$high, options(digits = 6))
DowData$low <- as.character(gsub("\\$", "", DowData$low))
DowData$low <- as.numeric(DowData$low, options(digits = 6))
DowData$close <- as.character(gsub("\\$", "", DowData$close))
DowData$close <- as.numeric(DowData$close, options(digits = 6))


# Change columns 12 & 13 to numerics
DowData$next_weeks_open <- as.character(gsub("\\$", "", DowData$next_weeks_open))
DowData$next_weeks_open <- as.numeric(DowData$next_weeks_open, options(digits = 6))

DowData$next_weeks_close <- as.character(gsub("\\$", "", DowData$next_weeks_open))
DowData$next_weeks_close <- as.numeric(DowData$next_weeks_close, options(digits = 6))

#Get lags for 1 week biweekly and monthly


# library(dplyr)
# DowData <- 
#   DowData %>%
#   group_by(PreviousClose) %>%
#   mutate(lag.value = dplyr::lag(value, n = 1, default = NA))

DowData <- DowData %>%
  group_by(stock) %>%
  mutate(percent_change_next_weeks_price_lag1 = lag(percent_change_price, n = 1, default = NA))

DowData <- DowData %>%
  group_by(stock) %>%
  mutate(percent_change_next_weeks_price_lag2 = lag(percent_change_price, n = 2, default = NA))

DowData <- DowData %>%
  group_by(stock) %>%
  mutate(percent_change_next_weeks_price_lag3 = lag(percent_change_price, n = 3, default = NA))

DowData <- DowData %>%
  group_by(stock) %>%
  mutate(percent_change_next_weeks_price_lag4 = lag(percent_change_price, n = 4, default = NA))

DowData <- DowData %>%
  group_by(stock) %>%
  mutate(percent_change_volume_lag1 = lag(percent_change_volume_over_last_wk, n = 1, default = NA))

DowData <- DowData %>%
  group_by(stock) %>%
  mutate(percent_change_volume_lag2 = lag(percent_change_volume_over_last_wk, n = 2, default = NA))

DowData <- DowData %>%
  group_by(stock) %>%
  mutate(percent_change_volume_lag3 = lag(percent_change_volume_over_last_wk, n = 3, default = NA))

DowData <- DowData %>%
  group_by(stock) %>%
  mutate(percent_change_volume_lag4 = lag(percent_change_volume_over_last_wk, n = 4, default = NA))


# Check variable correlation
corr_data <- DowData[,4:16]
corr_matrix <- cor(corr_data)
corrplot(corr_matrix, type = "upper")



# Split into training and testing
train <- which(DowData$quarter == 1)
dataTrain <- DowData[train,]
dataTest <- DowData[-train,]


# Set the formula
formula <- (percent_change_next_weeks_price ~ stock + open + close + high + low + volume + percent_change_price + 
              percent_change_volume_over_last_wk + previous_weeks_volume + days_to_next_dividend + percent_return_next_dividend + 
              percent_change_next_weeks_price_lag1 + percent_change_next_weeks_price_lag2 + percent_change_next_weeks_price_lag3 + 
              percent_change_next_weeks_price_lag4 + percent_change_volume_lag1 + percent_change_volume_lag2 +
              percent_change_volume_lag3 + percent_change_volume_lag4)

# Linear Model
lmModel <- lm(formula, data = dataTrain)

lmModel
summary(lmModel)

# Find significant predictors
lmstepData = na.omit(dataTrain)
steplmModel <- stepAIC(lm(formula, data = lmstepData), direction = "both")

steplmModel
summary(steplmModel)

predLM = predict(lmModel, dataTest)

plot(dataTest$percent_change_price, predLM, main = "Linear Model Actual vs Predicted",
     xlab = "Actual", ylab = "Predicted", col = 4)
abline(0,1)

mean((predLM - dataTest$percent_change_price)^2)


# Decision Tree
treeModel <- tree(formula,
                  data = dataTrain, subset = train) 

crossValTree = cv.tree(treeModel)
#best size = 21
optimalSize = which.min(crossValTree$size)

treeModel = prune.tree(treeModel, best = optimalSize)
plot(treeModel)
text(treeModel, pretty = 0)


#Predictions on test set
treePreds = predict(treeModel, dataTest)
plot(treePreds, dataTest$percent_change_price, xlab = "Prediction", ylab = "Percent Change in Price")
abline(0,1)

mean((treePreds - dataTest$percent_change_price)^2)




# SVM Model
tuned = tune.svm(formula, 
                 data = dataTrain,
                 gamma = seq(0.01, 0.1, by = 0.01),
                 cost = seq(.1, 1, by = .1))

svmModel = svm(formula, 
               data = dataTrain,
               gamma = tuned$best.parameters$gamma,
               cost = tuned$best.parameters$cost)

svmModel
summary(svmModel)

#Prediction on test set
svmpredict = predict(svmModel, dataTest, type = "response")

plot(dataTest$percent_change_price, svmpredict, xlab = "Actual", ylab = "Predicted", col = 4)
abline(svmpredict)

# error:
mean((svmpredict - dataTest$percent_change_price)^2)






###################### CAPM #################
# Compute the returns and remove any missing values. 
ReturnDow = (Delt(DowData[,7]))

DowData$ReturnDow=ReturnDow

DowData = na.omit(DowData)

colnames(DowData$ReturnDow) = "Dow Jones Weekly Returns"
head(DowData$ReturnDow)



#See how the data looks. You can see the risk... 
boxplot(DowData$ReturnDow, main="Expected Return", xlab="Dow Jones", ylab="Return")

# Compute mean and stdev for the returns.
DataMean=apply(DowData$ReturnDow, 2, mean)
DataSD=apply(DowData$ReturnDow, 2, sd)
# Take a look at the means and standard deviations. 
cbind(DataMean,DataSD)


# According to the CAPM formula, we will first get the beta of each stock by 
# regressions; then calculate the expected return of each stock and the covariance 
# matrix of the four stocks; finally we can calculate the optimal asset allocations 
# (weights) of the portfolio consisting of the 2 stocks.

# Model fit can be judged by R-sq. Using LM here as interprtation 
# is key. SVM is not so useful.


########## Stock = AA

AAdata <- subset(DowData, stock == "AA",
                  select=c(stock, close, ReturnDow))

lm.Dow.AA<- lm(AAdata$close
            ~ ReturnDow, data = as.data.frame(AAdata))
summary(lm.Dow.AA)
BetaAA <- summary(lm.Dow.AA)$coefficients[2, 1]
BetaAA


########## Stock = AXP

AXPdata <- subset(DowData, stock == "AXP",
                 select=c(stock, close, ReturnDow))

lm.Dow.AXP<- lm(AXPdata$close
               ~ ReturnDow, data = as.data.frame(AXPdata))
summary(lm.Dow.AXP)
BetaAXP <- summary(lm.Dow.AXP)$coefficients[2, 1]
BetaAXP

########## Stock = BA

BAdata <- subset(DowData, stock == "BA",
                 select=c(stock, close, ReturnDow))

lm.Dow.BA<- lm(BAdata$close
               ~ ReturnDow, data = as.data.frame(BAdata))
summary(lm.Dow.BA)
BetaBA <- summary(lm.Dow.BA)$coefficients[2, 1]
BetaBA

########## Stock = BAC

BACdata <- subset(DowData, stock == "BAC",
                 select=c(stock, close, ReturnDow))

lm.Dow.BAC<- lm(BACdata$close
               ~ ReturnDow, data = as.data.frame(BACdata))
summary(lm.Dow.BAC)
BetaBAC <- summary(lm.Dow.BAC)$coefficients[2, 1]
BetaBAC


########## Stock = CAT

CATdata <- subset(DowData, stock == "CAT",
                 select=c(stock, close, ReturnDow))

lm.Dow.CAT<- lm(CATdata$close
               ~ ReturnDow, data = as.data.frame(CATdata))
summary(lm.Dow.CAT)
BetaCAT <- summary(lm.Dow.CAT)$coefficients[2, 1]
BetaCAT


data.table(BetaAA, BetaAXP, BetaBA, BetaBAC, BetaCAT)
