# two variables: BER.old and BER.new

library(readr)
library(EnvStats)
library(nortest)

# set working directory 
setwd("C:/DataAnalytics/Lab1")

# read data; stores the csv as an R project/data table in the epi.data variable
epi.data <- read_csv("epi_results_2024_pop_gdp.csv")

# print summaries of each variable
summary(epi.data$BER.old)
summary(epi.data$BER.new)

### Explore Variable ###

## take a copy of the variables from the dataframe into separate variables
B1 <- epi.data$BER.old 
B2 <- epi.data$BER.new

# find NAs in BER.old
NAs <- is.na(B1)

# print
NAs

# get positions of NAs
rownums <- which(NAs)
rownums
B1[rownums]
# take subset of NOT NAs from BER.old
B1.complete <- B1[!NAs]
B1.complete

# get summary
summary(B1.complete)

# same for B2 (BER.new):

# find NAs in BER.new
NAs <- is.na(B2)

# print
NAs

# get positions of NAs
rownums <- which(NAs)
rownums
B2[rownums]
# take subset of NOT NAs from BER.old
B2.complete <- B2[!NAs]
B2.complete

# get summary
summary(B2.complete)

### Boxplot ###
boxplot(B1.complete, B2.complete, names  =c("BER.old", "BER.new"))


### Histograms ###

# histogram for B1.complete
x <- seq(0, 101, 5)
hist(B1.complete, x, prob=TRUE)
lines(density(B1.complete, bw="SJ"))
x1 <- seq(0, 101, 1)
d1 <- dnorm(x1, mean=mean(B1.complete), sd=sd(B1.complete), log=FALSE)
lines(x1, d1)

# histogram for B2.complete
x <- seq(0, 101, 5)
hist(B2.complete, x, prob=TRUE)
lines(density(B2.complete, bw="SJ"))
x1 <- seq(0, 101, 1)
d1 <- dnorm(x1, mean=mean(B2.complete), sd=sd(B2.complete), log=FALSE)
lines(x1, d1)

### ECDF plots ###
plot(ecdf(B1.complete), do.points=FALSE, verticals=TRUE)
plot(ecdf(B2.complete), do.points=FALSE, verticals=TRUE)

### QQ plots of each variable against the normal distribution ###
qqnorm(B1.complete); qqline(B1.complete)
qqnorm(B2.complete); qqline(B2.complete)

### QQ plots of each variable against each other ###
qqplot(B1.complete, B2.complete, xlab = "Q-Q plot for BER.old & BER.new") 

### Normality statistical tests ###
shapiro.test(B1.complete) # result: p < 0.05, reject the null hypothesis (not normally distributed)
shapiro.test(B2.complete) # result: p < 0.05, reject the null hypothesis (not normally distributed)

ad.test(B1.complete)
ad.test(B2.complete)
# result: p < 0.05 for both, reject the null hypothesis (neither are normally distributed)

### Test for identical distributions ###
ks.test(B1.complete, B2.complete) # result: p = 1 --> no evidence of difference in the distributions
wilcox.test(B1.complete, B2.complete) # result: p > 0.5 --> the median BER score didn't change between old and new

