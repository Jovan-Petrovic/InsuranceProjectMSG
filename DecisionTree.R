library(rpart)
library(rpart.plot)

str(train.data.new.boruta)

# build the model
tree1 <- rpart(Num.of.mobile.home.policies ~ ., data = train.data.new.boruta, method = "class")
print(tree1)
rpart.plot(tree1)

# make the predictions with tree1 over the test dataset
tree1.pred <- predict(object = tree1, newdata = test.data.new.boruta, type = "class")
head(tree1.pred)
tree1.pred

# create the confusion matrix
tree1.cm <- table(true=test.data.new.boruta$Num.of.mobile.home.policies, predicted=tree1.pred)
tree1.cm

# compute the evaluation metrics
tree1.eval <- compute.eval.metrics(tree1.cm)
tree1.eval


# cross-validation
library(e1071)
library(caret)

# define cross-validation (cv) parameters; we'll perform 10-fold crossvalidation
numFolds = trainControl( method = "cv", number = 10 )
# define the range for the cp values to examine in the cross-validation
cpGrid = expand.grid( .cp = seq(0.001, to = 0.05, by = 0.0025)) 
set.seed(10)
# run the cross-validation
dt.cv <- train(x = train.data.new.boruta[,-35],
               y = train.data.new.boruta$Num.of.mobile.home.policies,
               method = "rpart",
               control = rpart.control(minsplit = 10),
               trControl = numFolds,
               tuneGrid = cpGrid)
dt.cv
# plot the cross-validation results
plot(dt.cv)
optimal_cp <- dt.cv$bestTune$cp
optimal_cp <- 0.01

# build the model
tree2 <- rpart(Num.of.mobile.home.policies ~ ., data = train.data.new.boruta, method = "class",
               control = rpart.control(minsplit = 10, cp = optimal_cp))
print(tree2)
rpart.plot(tree2)

# make the predictions with tree3 over the test dataset
tree2.pred <- predict(tree2, newdata = test.data.new.boruta, type = "class")

# create the confusion matrix for tree3 predictions
tree2.cm <- table(true = test.data.new.boruta$Num.of.mobile.home.policies, predicted = tree2.pred)
tree2.cm

# compute the evaluation metrics
tree2.eval <- compute.eval.metrics(tree2.cm)
tree2.eval
