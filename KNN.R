####################################
### KNN with unbalanced dataset ###
####################################

str(train.data.new.boruta.under)

apply(X = train.data.new.boruta.under[,-35],
      MARGIN = 2,
      FUN = function(x) length(boxplot.stats(x)$out)) 

train.data.new.boruta.under.st <- apply(X = train.data.new.boruta[,-35],
                     MARGIN = 2,
                     FUN = function(x) scale(x, center = TRUE, scale =
                                               TRUE))
train.data.new.boruta.st <- as.data.frame(train.data.new.boruta.st)
train.data.new.boruta.st$Num.of.mobile.home.policies <- train.data.new.boruta$Num.of.mobile.home.policies
str(train.data.new.boruta.st)
summary(train.data.new.boruta.st)

library(class)
# create a knn model with k=5
set.seed(111)
knn.pred <- knn(train = train.data.new.boruta.standardized[,-35],
                test = test.data.new.boruta.standardized[,-35],
                cl = train.data.new.boruta.standardized$Num.of.mobile.home.policies,
                k = 11)
# print several predictions
head(knn.pred)

# create the confusion matrix
knn.cm <- table(predicted = knn.pred, true = test.data.new.boruta.standardized$Num.of.mobile.home.policies)
knn.cm

# compute the evaluation metrics
knn.eval <- compute.eval.metrics(knn.cm)
knn.eval

# cross-validation
library(e1071)
# define cross-validation (cv) parameters; we'll perform 10-fold crossvalidation
numFolds = trainControl( method = "cv", number = 10)
# define the range for the k values to examine in the cross-validation
cpGrid = expand.grid(.k = seq(from=3, to = 5, by = 2))

set.seed(10320)
# run the cross-validation
knn.cv <- train(x = train.data.new.boruta.st[,-35],
                y = train.data.new.boruta.st$Num.of.mobile.home.policies,
                method = "knn",
                trControl = numFolds,
                tuneGrid = cpGrid, )
knn.cv

# plot the cross-validation results
plot(knn.cv)

# build a new model with the best value for k
best_k <- knn.cv$bestTune$k
best_k
knn.pred2 <- knn(train = train.data.new.boruta.st[,-35],
                 test = test.data.new.boruta[,-35],
                 cl = train.data.new.boruta.st$Num.of.mobile.home.policies,
                 k = best_k)
# create the confusion matrix
knn.cm2 <- table(predicted = knn.pred2, true = test.data.new.boruta$Num.of.mobile.home.policies)
knn.cm2

# compute the evaluation metrics
knn.eval2 <- compute.eval.metrics(knn.cm2)
knn.eval2


####################################
### KNN with normalized dataset ###
####################################

# function for performing the normalization
normalize.feature <- function( feature ) {
  if ( sum(feature, na.rm = T) == 0 ) feature
  else ((feature - min(feature, na.rm = T))/(max(feature, na.rm = T) -
                                               min(feature, na.rm = T)))
}

str(train.data.new.boruta)

train.data.new.boruta.nm <- normalize.feature(feature = train.data.new.boruta[,-35])
summary(train.data.new.boruta.nm)
train.data.new.boruta.nm$Num.of.mobile.home.policies <- train.data.new.boruta$Num.of.mobile.home.policies
str(train.data.new.boruta.nm)

library(class)
# create a knn model with k=5
set.seed(111)
knn.pred <- knn(train = train.data.new.boruta.nm[,-35],
                test = test.data.new.boruta[,-35],
                cl = train.data.new.boruta.nm$Num.of.mobile.home.policies,
                k = 9)
# print several predictions
head(knn.pred)

# create the confusion matrix
knn.cm <- table(predicted = knn.pred, true = test.data.new.boruta$Num.of.mobile.home.policies)
knn.cm

# compute the evaluation metrics
knn.eval <- compute.eval.metrics(knn.cm)
knn.eval

# cross-validation
library(e1071)
# define cross-validation (cv) parameters; we'll perform 10-fold crossvalidation
numFolds = trainControl( method = "cv", number = 10)
# define the range for the k values to examine in the cross-validation
cpGrid = expand.grid(.k = seq(from=3, to = 5, by = 2))

set.seed(10320)
# run the cross-validation
knn.cv <- train(x = train.data.new.boruta.st[,-35],
                y = train.data.new.boruta.st$Num.of.mobile.home.policies,
                method = "knn",
                trControl = numFolds,
                tuneGrid = cpGrid, )
knn.cv

# plot the cross-validation results
plot(knn.cv)

# build a new model with the best value for k
best_k <- knn.cv$bestTune$k
best_k
knn.pred2 <- knn(train = train.data.new.boruta.st[,-35],
                 test = test.data.new.boruta[,-35],
                 cl = train.data.new.boruta.st$Num.of.mobile.home.policies,
                 k = best_k)
# create the confusion matrix
knn.cm2 <- table(predicted = knn.pred2, true = test.data.new.boruta$Num.of.mobile.home.policies)
knn.cm2

# compute the evaluation metrics
knn.eval2 <- compute.eval.metrics(knn.cm2)
knn.eval2


######################################
### KNN with undersampled dataset ###
#####################################

str(train.data.new.boruta)

apply(X = train.data.new.boruta[,-35],
      MARGIN = 2,
      FUN = function(x) length(boxplot.stats(x)$out)) 

train.data.new.boruta.st <- apply(X = train.data.new.boruta[,-35],
                                  MARGIN = 2,
                                  FUN = function(x) scale(x, center = TRUE, scale =
                                                            TRUE))
train.data.new.boruta.st <- as.data.frame(train.data.new.boruta.st)
train.data.new.boruta.st$Num.of.mobile.home.policies <- train.data.new.boruta$Num.of.mobile.home.policies
str(train.data.new.boruta.st)
summary(train.data.new.boruta.st)

library(class)
# create a knn model with k=5
set.seed(111)
knn.pred <- knn(train = train.data.new.boruta.under.standardized[,-35],
                test = test.data.new.boruta.under.standardized[,-35],
                cl = train.data.new.boruta.under.standardized$Num.of.mobile.home.policies,
                k = 49)
# print several predictions
head(knn.pred)

# create the confusion matrix
knn.cm <- table(predicted = knn.pred, true = test.data.new.boruta.under.standardized$Num.of.mobile.home.policies)
knn.cm

# compute the evaluation metrics
knn.eval <- compute.eval.metrics(knn.cm)
knn.eval

# cross-validation
library(e1071)
# define cross-validation (cv) parameters; we'll perform 10-fold crossvalidation
numFolds = trainControl( method = "cv", number = 10)
# define the range for the k values to examine in the cross-validation
cpGrid = expand.grid(.k = seq(from=3, to = 5, by = 2))

set.seed(10320)
# run the cross-validation
knn.cv <- train(x = train.data.new.boruta.st[,-35],
                y = train.data.new.boruta.st$Num.of.mobile.home.policies,
                method = "knn",
                trControl = numFolds,
                tuneGrid = cpGrid, )
knn.cv

# plot the cross-validation results
plot(knn.cv)

# build a new model with the best value for k
best_k <- knn.cv$bestTune$k
best_k
knn.pred2 <- knn(train = train.data.new.boruta.st[,-35],
                 test = test.data.new.boruta[,-35],
                 cl = train.data.new.boruta.st$Num.of.mobile.home.policies,
                 k = best_k)
# create the confusion matrix
knn.cm2 <- table(predicted = knn.pred2, true = test.data.new.boruta$Num.of.mobile.home.policies)
knn.cm2

# compute the evaluation metrics
knn.eval2 <- compute.eval.metrics(knn.cm2)
knn.eval2

###############################################
### KNN with oversampled dataset ###
##############################################

library(class)
# create a knn model with k=5
set.seed(111)
knn.pred <- knn(train = train.data.new.boruta.over.standardized[,-35],
                test = test.data.new.boruta.over.standardized[,-35],
                cl = train.data.new.boruta.over.standardized$Num.of.mobile.home.policies,
                k = 29)
# print several predictions
head(knn.pred)

# create the confusion matrix
knn.cm <- table(predicted = knn.pred, true = test.data.new.boruta.over.standardized$Num.of.mobile.home.policies)
knn.cm

# compute the evaluation metrics
knn.eval <- compute.eval.metrics(knn.cm)
knn.eval

############################################################
### KNN with unbalanced improved standardization dataset ###
###########################################################

library(class)
# create a knn model with k=5
set.seed(111)
knn.pred <- knn(train = train.data.new.boruta.standardized[,-35],
                test = test.data.new.boruta.standardized[,-35],
                cl = train.data.new.boruta.standardized$Num.of.mobile.home.policies,
                k = 3)
# print several predictions
head(knn.pred)

# create the confusion matrix
knn.cm <- table(predicted = knn.pred, true = test.data.new.boruta$Num.of.mobile.home.policies)
knn.cm

# compute the evaluation metrics
knn.eval <- compute.eval.metrics(knn.cm)
knn.eval
