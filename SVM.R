####################################
### SVM with unbalanced dataset ###
####################################

library(e1071)
set.seed(111)
mymodel <- svm(Num.of.mobile.home.policies ~ ., data = train.data.new.boruta.standardized, 
              kernel = "radial")
summary(mymodel)

svm.pred <- predict(mymodel, test.data.new.boruta.standardized) 
tab <- table(Predicted = svm.pred, Actual = test.data.new.boruta.standardized$Num.of.mobile.home.policies)
tab

svm.eval <- compute.eval.metrics(tab)
svm.eval

# Tuning
set.seed(123)
tmodel <- tune(svm, Num.of.mobile.home.policies ~ ., data = train.data.new.boruta.standardized,
     ranges = list(epsilon = seq(0,0.3,0.1), cost = 2^(2:3)))
dev.new(width=10, height=10)
plot(tmodel)
summary(tmodel)

mymodel <- tmodel$best.model
summary(mymodel)

set.seed(111)
svm.pred <- predict(mymodel, test.data.new.boruta.standardized) 
tab <- table(Predicted = svm.pred, Actual = test.data.new.boruta.standardized$Num.of.mobile.home.policies)
tab

svm.eval <- compute.eval.metrics(tab)
svm.eval

####################################
### SVM with undersampled dataset ###
####################################

library(e1071)
set.seed(111)
mymodel <- svm(Num.of.mobile.home.policies ~ ., data = train.data.new.boruta.under.standardized, 
               kernel = "sigmoid")
summary(mymodel)

svm.pred <- predict(mymodel, test.data.new.boruta.under.standardized) 
tab <- table(Predicted = svm.pred, Actual = test.data.new.boruta.under.standardized$Num.of.mobile.home.policies)
tab

svm.eval <- compute.eval.metrics(tab)
svm.eval

# Tuning
set.seed(123)
tmodel <- tune(svm, Num.of.mobile.home.policies ~ ., data = train.data.new.boruta.under.standardized,
               ranges = list(epsilon = seq(0,0.7,0.1), cost = 2^(2:5)))
dev.new(width=10, height=10)
plot(tmodel)
summary(tmodel)

mymodel <- tmodel$best.model
summary(mymodel)

set.seed(111)
svm.pred <- predict(mymodel, test.data.new.boruta.under.standardized) 
tab <- table(Predicted = svm.pred, Actual = test.data.new.boruta.under.standardized$Num.of.mobile.home.policies)
tab

svm.eval <- compute.eval.metrics(tab)
svm.eval



####################################
### SVM with oversampled dataset ###
####################################

library(e1071)
set.seed(111)
mymodel <- svm(Num.of.mobile.home.policies ~ ., data = train.data.new.boruta.over.standardized, 
               kernel = "sigmoid")
summary(mymodel)

svm.pred <- predict(mymodel, test.data.new.boruta.over.standardized) 
tab <- table(Predicted = svm.pred, Actual = test.data.new.boruta.over.standardized$Num.of.mobile.home.policies)
tab

svm.eval <- compute.eval.metrics(tab)
svm.eval

# Tuning
set.seed(123)
tmodel <- tune(svm, Num.of.mobile.home.policies ~ ., data = train.data.new.boruta.under.standardized,
               ranges = list(epsilon = seq(0,0.7,0.1), cost = 2^(2:5)))
dev.new(width=10, height=10)
plot(tmodel)
summary(tmodel)

mymodel <- tmodel$best.model
summary(mymodel)

set.seed(111)
svm.pred <- predict(mymodel, test.data.new.boruta.under.standardized) 
tab <- table(Predicted = svm.pred, Actual = test.data.new.boruta.under.standardized$Num.of.mobile.home.policies)
tab

svm.eval <- compute.eval.metrics(tab)
svm.eval