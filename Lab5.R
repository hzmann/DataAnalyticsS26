#######
# LAB 5
#######

##############################################
# Load dataset and assign correct column names
##############################################
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

############################################################
# Choose subset of variables (based on PCA-style importance):
# Using 4 strong variables (same idea as lab 5)
############################################################

selected_vars <- c("Flavanoids", "Total_phenols", "OD280_OD315", "Proline")

##################################
# Train/test split (for all models)
##################################
set.seed(123)

n <- nrow(wine)
train_index <- sample(1:n, size = round(0.7 * n))
test_index  <- setdiff(1:n, train_index)

train_x <- wine[train_index, selected_vars]
test_x  <- wine[test_index, selected_vars]

train_y <- wine$Class[train_index]
test_y  <- wine$Class[test_index]

##################################
# Scale features (for SVM and kNN)
##################################

train_means <- apply(train_x, 2, mean)
train_sds   <- apply(train_x, 2, sd)

train_x_scaled <- scale(train_x, center = train_means, scale = train_sds)
test_x_scaled  <- scale(test_x, center = train_means, scale = train_sds)

#####################################################
# Helper function to compute Precision / Recall / F1
#####################################################

classification_metrics <- function(actual, predicted) {
  # ensure both actual and predicated are factors with the same levels
  actual <- factor(actual, levels = levels(actual))
  predicted <- factor(predicted, levels = levels(actual))
  
  # create confusion matrix
  cm <- table(actual, predicted)
  # get list of class labels
  classes <- rownames(cm)
  
  # initialize vectors to store metrics for each class
  precision <- numeric(length(classes))
  recall <- numeric(length(classes))
  f1 <- numeric(length(classes))
  
  # loop through each class and compute metrics from confusion matrix
  for (i in 1:length(classes)) {
    # true pos, false pos, false negs
    tp <- cm[i, i]
    fp <- sum(cm[, i]) - tp
    fn <- sum(cm[i, ]) - tp
    
    # precision and recall
    precision[i] <- ifelse((tp + fp) == 0, NA, tp / (tp + fp))
    recall[i]    <- ifelse((tp + fn) == 0, NA, tp / (tp + fn))
    
    # f1 score
    f1[i] <- ifelse(
      is.na(precision[i]) || is.na(recall[i]) || (precision[i] + recall[i]) == 0,
      NA,
      2 * precision[i] * recall[i] / (precision[i] + recall[i])
    )
  }
  
  # combine results into a clean table
  metrics <- data.frame(
    Class = classes,
    Precision = precision,
    Recall = recall,
    F1 = f1
  )
  
  return(list(confusion_matrix = cm, metrics = metrics))
}

############################################################
# Train 2 SVM classifiers to predict the type of wine
# One linear + one another kernel
# Use tune.svm to find optimal C and gamma
############################################################

library(e1071)

############################
# Linear SVM (tuning C only)
############################

# use tune.vsm to find best hyperparemters for linear SVM
tune_linear <- tune.svm(
  # train scaled feature data; rows are wine samples, columns are selected features
  x = train_x_scaled,
  y = train_y,
  kernel = "linear",
  # list of candidate cost values to test; smaller --> simpler boundary, more mistakes
  # larger c --> more complex boundary, aims to classify everything correctly
  cost = c(0.01, 0.1, 1, 10, 100)
)

# extract best performing
best_linear <- tune_linear$best.model

# use trained best model to make predictions on test data
pred_linear <- predict(best_linear, test_x_scaled)

# compute cm, precision, recall, and f1 for the predictions 
results_linear <- classification_metrics(test_y, pred_linear)

cat("SVM Model 1: Linear Kernel\n")

cat("\nBest Parameters:\n")
print(tune_linear$best.parameters)

cat("\nConfusion Matrix:\n")
print(results_linear$confusion_matrix)

cat("\nPrecision / Recall / F1:\n")
print(results_linear$metrics)

######################################################################
# Radial (RBF) SVM: a common and effective non-linear extension of SVM
######################################################################

# tuen an SVM with a radial/RBF kernel to find best hyperparameters
tune_rbf <- tune.svm(
  # train scaled feature data
  x = train_x_scaled,
  # true class labels fro training data
  y = train_y,
  kernel = "radial",
  # candidate cost values
  cost = c(0.1, 1, 10, 100),
  # candidate gamma values: smaller gamma --> smoother, broader boundaries
  # larger gamma --> tighter boundaries, more complex
  gamma = c(0.01, 0.1, 1, 10)
)

# extract best performing model
best_rbf <- tune_rbf$best.model

# use trained best model to make predictions on the test data
pred_rbf <- predict(best_rbf, test_x_scaled)

# compute cm, precision, recall, and f1
results_rbf <- classification_metrics(test_y, pred_rbf)

cat("SVM Model 2: Radial (RBF) Kernel\n")

cat("\nBest Parameters:\n")
print(tune_rbf$best.parameters)

cat("\nConfusion Matrix:\n")
print(results_rbf$confusion_matrix)

cat("\nPrecision / Recall / F1:\n")
print(results_rbf$metrics)

############################################################
# Train another classifier (kNN) on same features
############################################################

library(class)

# use k-nearets neighbors to classify test data
pred_knn <- knn(
  train = train_x_scaled,
  test = test_x_scaled,
  cl = train_y,
  # look at 5 closest training points and assign the majority class
  k = 5
)

results_knn <- classification_metrics(test_y, pred_knn)

cat("Model 3: kNN (k = 5)\n")

cat("\nConfusion Matrix:\n")
print(results_knn$confusion_matrix)

cat("\nPrecision / Recall / F1:\n")
print(results_knn$metrics)

############################################################
# Compare performance of all models
############################################################

cat("FINAL COMPARISON OF MODELS\n")

cat("\n--- Linear SVM ---\n")
print(results_linear$metrics)

cat("\n--- RBF SVM ---\n")
print(results_rbf$metrics)

cat("\n--- kNN ---\n")
print(results_knn$metrics)

# the linear SVM performs best overall, achieving consistently high precision, 
# recall, and F1 across all classes at .91-1.0, .91-1.0, and .95-.97 for precision, recall, and f1, respectively.
# the RBF SVM is still strong, but performs slightly worse than the linear SVM at metrics of .9-1.0, .87-1.0, and .93-.95 
# for precision, recall, and f1, respectively. This suggests the added non-linear flexibility
# didn't provide a meaningful advantage for this dataset. The kNN model performs the weakest, especially with 
# noticeably lower recall for class 2 and precision for class 1 idicating more classifications, but otherwise performing
# moderately well. These results suggest the data is largely linearly separable, making the linear SVM
# most effective.
