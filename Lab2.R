library("ggplot2")
library("readr")

## read dataset
dataset <- read_csv("C:/DataAnalytics/Lab2/NY-House-Dataset.csv")

## filter data
dataset <- dataset[dataset$PRICE<195000000,]
dataset <- dataset[dataset$PRICE>10000,]

# Log transform each of the vars
dataset$logPrice <- log10(dataset$PRICE)
dataset$logSqFt <- log10(dataset$PROPERTYSQFT)
dataset$logBeds <- log10(dataset$BEDS)
dataset$logBath <- log10(dataset$BATH)

## Model 1: Price ~ PropertySqFt
model1 <- lm(logPrice ~ logSqFt, data = dataset)
# print model summary stats
summary(model1) # we got an extremely small p value so PropertySqFt is extremely significant
# plot most significant variable
ggplot(dataset, aes(x = logSqFt, y = logPrice)) +
  geom_point() + stat_smooth(method="lm", col="red") +
  ggtitle("Model 1: Price vs MostSig")
# residual plot
ggplot(model1, aes(x = .fitted, y = .resid)) + geom_point() +
  geom_hline(yintercept=0) + ggtitle("Model 1 Residual Plot")


## Model 2: Price ~ PropertySqFt + Beds
model2 <- lm(logPrice ~ logSqFt + logBeds, data=dataset)
# print model summary stats
summary(model2) # p vals are the same but logSqFt t val is > logBeds
# plot most significant variable 
ggplot(dataset, aes(x = logSqFt, y = logPrice)) +
  geom_point() + stat_smooth(method="lm", col="red") +
  ggtitle("Model 2: Price vs logSqFt")
# residual plot
ggplot(model2, aes(x = .fitted, y = .resid)) + geom_point() +
  geom_hline(yintercept=0) + ggtitle("Model 2 Residual Plot")

## Model 3: Price ~ PropertySqFt + Beds + Bath
model3 <- lm(logPrice ~ logSqFt + logBeds + logBath, data=dataset)
# print model summary stats
summary(model3) # smallest p and largest t: logBath
# plot most significant variable 
ggplot(dataset, aes(x = logBath, y = logPrice)) +
  geom_point() + stat_smooth(method="lm", col="red") +
  ggtitle("Model 3: Price vs logBath")
# residual plot
ggplot(model3, aes(x = .fitted, y = .resid)) + geom_point() +
  geom_hline(yintercept=0) + ggtitle("Model 3 Residual Plot")

## Determine most useful model by comparing R-squared
# model 1: 0.3483
# model 2: 0.3423
# model 3: 0.5103
# model 3 has the highest R-squared meaning it has the best fit and captures data best
