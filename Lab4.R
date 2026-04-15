#########
# LAB 4
#########

############################################################
# Load the dataset and assign correct column names
############################################################

# dataset has no header row so add names manually
wine <- read.csv("wine.data", header = FALSE)

colnames(wine) <- c(
  "Class",
  "Alcohol",
  "Malic_acid",
  "Ash",
  "Alcalinity_of_ash",
  "Magnesium",
  "Total_phenols",
  "Flavanoids",
  "Nonflavanoid_phenols",
  "Proanthocyanins",
  "Color_intensity",
  "Hue",
  "OD280_OD315",
  "Proline"
)

# make the class variable a factor for classification
wine$Class <- factor(wine$Class)

###############################################################
# Compute the PCs and plot the dataset using the 1st and 2nd PCs
###############################################################

# PCA should be done on the predictor variables only, not the class column
# we standardize because the variables are on different scales
wine_x <- wine[, -1]

wine_pca <- princomp(wine_x, cor = TRUE)

# plot the dataset using the first two principal components
plot(
  wine_pca$scores[, 1],
  wine_pca$scores[, 2],
  col = as.numeric(wine$Class),
  pch = 19,
  xlab = "PC1",
  ylab = "PC2",
  main = "Wine Data Projected onto the First Two Principal Components"
)

legend(
  "topright",
  legend = levels(wine$Class),
  col = 1:length(levels(wine$Class)),
  pch = 19,
  title = "Wine Type"
)

##############################################################
# Identify the variables that contribute the most to the 1st PC
##############################################################

# loadings show how strongly each original variable contributes to each PC
pc1_loadings <- loadings(wine_pca)[, 1]

# sort by absolute value so the strongest contributors appear first
pc1_contrib <- sort(abs(pc1_loadings), decreasing = TRUE)

# print actual PC1 loadings
cat("PC1 loadings:\n")
print(pc1_loadings)

cat("\nVariables contributing most to PC1:\n")
print(pc1_contrib) # Flavanoids, Total_phenols, OD280_OD315, Proanthocyanins

##########################################
# choose 4 variables for first classifier:
# using top 4 contributors to PC1
##########################################
top4_vars <- names(pc1_contrib)[1:4]

cat("\nTop 4 variables selected for the original-variable classifier:\n")
print(top4_vars) # Flavanoids, Total_phenols, OD280_OD315, Proanthocyanins

##############################################
# setup for both classification models
##############################################

# will use the same train/test split for both models
set.seed(123)

n <- nrow(wine)
train_index <- sample(1:n, size = round(0.7 * n))
test_index  <- setdiff(1:n, train_index)

train_y <- wine$Class[train_index]
test_y  <- wine$Class[test_index]

############################################################
# Helper function:
# Compute precision, recall, and F1 from a contingency table
############################################################

classification_metrics <- function(actual, predicted) {
  actual <- factor(actual, levels = levels(actual))
  predicted <- factor(predicted, levels = levels(actual))
  
  cm <- table(actual, predicted)
  classes <- rownames(cm)
  
  precision <- numeric(length(classes))
  recall <- numeric(length(classes))
  f1 <- numeric(length(classes))
  
  # compute precision, recall, and F1 score for each class individually from confusion matrix
  for (i in 1:length(classes)) {
    # true pos, false pos, false negs
    tp <- cm[i, i]
    fp <- sum(cm[, i]) - tp
    fn <- sum(cm[i, ]) - tp
    
    # compute precision: precision = tp/(tp + fp)
    if ((tp + fp) == 0) {
      precision[i] <- NA
    } else {
      precision[i] <- tp / (tp + fp)
    }
    
    # compute recall: recall = tp/(tp + fn)
    if ((tp + fn) == 0) {
      recall[i] <- NA
    } else {
      recall[i] <- tp / (tp + fn)
    }
    
    # compute f1 score: f1 = 2 * (precision * recall)/(precision + recall)
    if (is.na(precision[i]) || is.na(recall[i]) || (precision[i] + recall[i]) == 0) {
      f1[i] <- NA
    } else {
      f1[i] <- 2 * precision[i] * recall[i] / (precision[i] + recall[i])
    }
  }
  
  metrics <- data.frame(
    Class = classes,
    Precision = precision,
    Recall = recall,
    F1 = f1
  )
  
  return(list(confusion_matrix = cm, metrics = metrics))
}

############################################################
# Train a classifier model (kNN) using a subset (3-4) of the
# variables in the original dataset
############################################################

# use the 4 selected original variables
train_x_orig <- wine[train_index, top4_vars]
test_x_orig  <- wine[test_index, top4_vars]

# kNN requires scaled variables
train_means <- apply(train_x_orig, 2, mean)
train_sds   <- apply(train_x_orig, 2, sd)

train_x_orig_scaled <- scale(train_x_orig, center = train_means, scale = train_sds)
test_x_orig_scaled  <- scale(test_x_orig, center = train_means, scale = train_sds)

# load standard kNN function
library(class)

# train/predict with kNN
pred_orig <- knn(
  train = train_x_orig_scaled,
  test = test_x_orig_scaled,
  cl = train_y,
  k = 5
)

# contingency table and metrics for original-variable model
results_orig <- classification_metrics(test_y, pred_orig)

cat("Model 1: kNN using selected original variables\n")
cat("Variables used:\n")
print(top4_vars)

cat("\nContingency table:\n")
print(results_orig$confusion_matrix)

cat("\nPrecision / Recall / F1:\n")
print(results_orig$metrics)

##########
# analysis:
# Model 1 performs perfectly on class 3, achieving 100% precision, recall, and F1
# however, there's significant confusion between class1 and class2, where many class 2
# samples are misclassified as class 1 (10), leading to lower recall for class 2 and 
# lower precision for class 1. While the chosen features are highly effective for 
# distinguishing class 3, they're far less effective at separating classes 1 and 2. 
# Overall, strong but uneven performance
##########

############################################################
# Train a classifier model to predict the wine type using the
# data projected onto the first 2 PCs
############################################################

# PCA for classification is fit using training data only
# so the test data is projected using the training PCA model
train_x_all <- wine[train_index, -1]
test_x_all  <- wine[test_index, -1]

train_pca <- princomp(train_x_all, cor = TRUE)

# training scores on the first 2 PCs
train_pc2 <- train_pca$scores[, 1:2]

# project test data onto the training PCA directions
test_x_all_scaled <- scale(
  test_x_all,
  center = train_pca$center,
  scale = train_pca$scale
)

test_pc2 <- test_x_all_scaled %*% train_pca$loadings[, 1:2]

# train/predict with kNN using the first 2 PCs
pred_pc <- knn(
  train = train_pc2,
  test = test_pc2,
  cl = train_y,
  k = 5
)

# contingency table and metrics for PC-based model
results_pc <- classification_metrics(test_y, pred_pc)

cat("Model 2: kNN using the first 2 principal components\n")

cat("\nContingency table:\n")
print(results_pc$confusion_matrix)

cat("\nPrecision / Recall / F1:\n")
print(results_pc$metrics)
###########
# Analysis:
# Model 2 achieves perfect classification across all three classes, with
# 100% precision, recall, and F1 for each class, indicating no misclassification in
# the test set. This reveals the first two principal components capture enough of the
# underlying structure of the data to completely separate the wine classes. Compared to 
# Model1, which struggled to distinguish between classes 1 and 2, the PCA-based model
# provides a much clearer separation between all classes, both simplifying the feature
# space and also significantly improving classification performance for this dataset.
###########

############################################################
# Compare the 2 classification models using contingency tables
# and precision/recall/F1 metrics
############################################################

cat("Comparison of the two models\n")

cat("\nModel 1 contingency table (original variables):\n")
print(results_orig$confusion_matrix)

cat("\nModel 1 precision / recall / F1:\n")
print(results_orig$metrics)

cat("\nModel 2 contingency table (first 2 PCs):\n")
print(results_pc$confusion_matrix)

cat("\nModel 2 precision / recall / F1:\n")
print(results_pc$metrics)

# Model 2 clearly outperforms Model 1, achieving perfect classification with no 
# errors in the contingency talbe and 100% precision, recall, and F1 across all classes.
# In contrast, Model 1 shows significant confusion between Classes 1 and 2, leading to lower
# precision for Class 1 and lower recall for Class 2, but performs perfectly on Class 3.
# This indicates the original selected features weren't sufficient to fully separate all classes,
# while the first two principal components provide a much cleaner separation. The PCA model is
# simpler with fewer feature and more accurate, which demonstrates the effectiveness of dimensionality
# for this dataset.