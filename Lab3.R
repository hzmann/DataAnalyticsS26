## LAB 3 ##

----------------
## EXERCISE 1 ##
----------------

library(class)
library(cluster)

# read data
abalone <- read.csv("C:/DataAnalytics/Lab3/abalone.data", header=FALSE)

## rename columns
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_weight', 'viscera_wieght', 'shell_weight', 'rings' ) 

## create target/age group
abalone$age.group <- cut(abalone$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))

## remove sex and rings
abalone.sub <- abalone[,c(2:8,10)]
abalone.sub$age.group <- as.character(abalone.sub$age.group)
abalone.sub$age.group <- as.factor(abalone.sub$age.group)

## 70/30 train-test split
train.indexes <- sample(4177,0.7*4177)
train <- abalone.sub[train.indexes,]
test <- abalone.sub[-train.indexes,]

## separate x (features) & y (class labels)

# Model 1: use length, diameter, height
Xtrain1 <- train[,1:3] 
Xtest1 <- test[,1:3]
# Model 2: whole_weight, shucked_weight, viscera_weight, shell_weight
Xtrain2 <- train[, 4:7]
Xtest2 <- test[, 4:7]

Ytrain <- train$age.group
Ytest <- test$age.group

## kNN requires scaling
# scale model 1
Xtrain1 <- scale(Xtrain1) # stores training means and training sds
# make test set use the training means and training sds: use train to define normal, then apply normal to test
Xtest1 <- scale(Xtest1, center=attr(Xtrain1,"scaled:center"), scale=attr(Xtrain1,"scaled:scale"))

## train the two models
# store the model predictions
preds1 <- knn(train=Xtrain1, test=Xtest1, cl=Ytrain, k=5) # chose 5 nearest to strike balance
preds2 <- knn(train=Xtrain2, test=Xtest2, cl=Ytrain, k=5)

## contingency tables
# model 1
table(preds1, Ytest) # confusion matrix: counts how many times predicted matches true
acc1 <- sum(preds1 == Ytest) / length(Ytest)
acc1 # 0.5789474
# model 2
table(preds2, Ytest)
acc2 <- sum(preds2 == Ytest) / length(Ytest)
acc2 # 0.6483254

## tuning K for model 2
k.vals <- 1:20 # we will try k's 1-20
# create empty numeric vector for each of the k values we're testing
accuracies <- numeric(length(k.vals))

for (i in 1:length(k.vals)){
  preds <- knn(train=Xtrain1, test=Xtest1, cl=Ytrain, k=k.vals[i])
  accuracies[i] <- sum(preds == Ytest)/length(Ytest)
}

# get the best k value
best.k <- k.vals[which.max(accuracies)]
best.k # best k was 1.. shocker
max(accuracies)

----------------
## EXERCISE 2 ##
----------------
# using model 2 subset: whole_weight, shucked_weight, viscera_weight, shell_weight
X <- train[, 4:7]
# scale
X <- scale(X)

## find optimal number of clusters k using silhouette
sil.kmeans <- numeric(10)
# k= 1 invalid for silhouette so start at 2
for(k in 2:11){
  # centers = k: # of clusters; nstart = 25: run k-means 25 times with different random points
  km.result <- kmeans(X, centers=k, nstart=25)
  # silhouette score for each observation
  silscore <- silhouette(km.result$cluster, dist(X))
  # store score for that k value
  sil.kmeans[k-1] <- mean(silscore[,3]) # silhouette width for each observation
}
best.k.means <- which.max(sil.kmeans) + 1
best.k.means # k = 2

## plot k means with best k val
km.best <- kmeans(X, centers=best.k.means, nstart=25)
sil.km.best <- silhouette(km.best$cluster, dist(X))
plot(sil.km.best, main="k-means plot with optimal k")

## find optimal k for PAM
sil.pam <- numeric(10)
for(k in 2:11){
  pam.model <- pam(X, k)
  sil.pam[k-1] <- pam.model$silinfo$avg.width
}
best.k.pam <- which.max(sil.pam)+1
best.k.pam # k=2

## Plot PAM with best k
pam.best <- pam(X, best.k.pam)
plot(pam.best, main="PAM with optimal k")