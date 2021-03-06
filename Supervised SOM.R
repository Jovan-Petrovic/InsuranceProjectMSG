##########################
### Unbalanced dataset ###
#########################

#Normalization
train.data.new.boruta.ssom.normalizedX <- scale(train.data.new.boruta[,-35])
# normalizing test dataset using mean and sd from train dataset
test.data.new.boruta.ssom.normalizedX <- scale(test.data.new.boruta[,-35], 
                                              center = attr(train.data.new.boruta.ssom.normalizedX, "scaled:center"),
                                              scale = attr(train.data.new.boruta.ssom.normalizedX, "scaled:scale"))
train.data.new.boruta.ssom.normalizedY <- train.data.new.boruta[,35]
Y <- test.data.new.boruta[,35]
test.data.new.boruta.targetzero <- test.data.new.boruta
test.data.new.boruta.targetzero[,35] <- 0
test.data.new.boruta.ssom.normalizedXY <- list(independent = test.data.new.boruta.ssom.normalizedX, dependent = test.data.new.boruta.targetzero[,35])

# Classification and Prediction Model
library(kohonen)
set.seed(111)
map.ssom <- xyf(train.data.new.boruta.ssom.normalizedX, 
                classvec2classmat(factor(train.data.new.boruta.ssom.normalizedY)),
                grid = somgrid(5,5,"hexagonal"),
                rlen = 100)
# matrix 1 is based on data from independent variables
# matrix 2 is based on data from dependet variable
plot(map.ssom, type = 'changes')
plot(map.ssom)
plot(map.ssom, type = 'count')

# Prediction
# prediction are going to be based training data
pred <- predict(map.ssom)
pred

# To see how good the model is we need to do predictions based on test data
pred.test <- predict(map.ssom, newdata = test.data.new.boruta.ssom.normalizedXY)
pred.test
pred.test.cm <- table(Predicted = pred.test$predictions[[2]], Actual = Y)
pred.test.cm

pred.test.eval <- compute.eval.metrics(pred.test.cm)
pred.test.eval

# Cluster Boundaries
par(mfrow = c(1,2))
# first plot is for independant variables, second is for dependant variables
plot(map.ssom, type = 'codes', main = c("Unsupervised SOM", "Supervised SOM"))
map.ssom.hc <- cutree(hclust(dist(map.ssom$codes[[2]])), 2)
add.cluster.boundaries(map.ssom, map.ssom.hc)
par(mfrow = c(1,1))

# clustering
c.class <- kmeans(map.ssom$codes[[2]], 3)
par(mfrow = c(1,2))
plot(map.ssom, type = "codes", main = c("Unsupervised SOM", "Supervised SOM"), 
     bgcol = rainbow(3)[c.class$cluster])
add.cluster.boundaries(map.ssom, c.class$cluster)
par(mfrow = c(1,1))

###########################
### Undersampled dataset ###
###########################

#Normalization
train.data.new.boruta.under.ssom.normalizedX <- scale(train.data.new.boruta.under[,-35])
# we will normalized test dataset using mean and sd from train dataset
test.data.new.boruta.ssom.normalizedX <- scale(test.data.new.boruta[,-35], 
                                               center = attr(train.data.new.boruta.under.ssom.normalizedX, "scaled:center"),
                                               scale = attr(train.data.new.boruta.under.ssom.normalizedX, "scaled:scale"))
train.data.new.boruta.under.ssom.normalizedY <- train.data.new.boruta.under[,35]
Y.under <- test.data.new.boruta[,35]
test.data.new.boruta.targetzero <- test.data.new.boruta
test.data.new.boruta.targetzero[,35] <- 0
test.data.new.boruta.ssom.normalizedXY <- list(independent = test.data.new.boruta.ssom.normalizedX, dependent = test.data.new.boruta.targetzero[,35])
# Classification and Prediction Model
set.seed(222)
map.under.ssom <- xyf(train.data.new.boruta.under.ssom.normalizedX, 
                classvec2classmat(factor(train.data.new.boruta.under.ssom.normalizedY)),
                grid = somgrid(5,5,"hexagonal"),
                rlen = 100)
# matrix 1 is based on data from independent variables
# matrix 2 is based on data from dependet variable
plot(map.under.ssom, type = 'changes')
plot(map.under.ssom)
plot(map.under.ssom, type = 'count')

# Prediction
# prediction are going to be based training data
pred.under <- predict(map.under.ssom)
pred.under

# To see how good the model is we need to do predictions based on test data
pred.test.under <- predict(map.under.ssom, newdata = test.data.new.boruta.ssom.normalizedXY)
pred.test.under.cm <- table(Predicted = pred.test.under$predictions[[2]], Actual = Y.under)

pred.test.under.eval <- compute.eval.metrics(pred.test.under.cm)
pred.test.under.eval

# Cluster Boundaries
par(mfrow = c(1,2))
# first plot is for independant variables, second is for dependant variables
plot(map.under.ssom, type = 'codes', main = c("Codes X", "Codes Y"))
map.under.ssom.hc <- cutree(hclust(dist(map.under.ssom$codes[[2]])), 2)
add.cluster.boundaries(map.under.ssom, map.under.ssom.hc)



###########################
### Oversampled dataset ###
###########################

#Normalization
train.data.new.boruta.over.ssom.normalizedX <- scale(train.data.new.boruta.over[,-35])
# we will normalized test dataset using mean and sd from train dataset
test.data.new.boruta.ssom.normalizedX <- scale(test.data.new.boruta[,-35], 
                                               center = attr(train.data.new.boruta.over.ssom.normalizedX, "scaled:center"),
                                               scale = attr(train.data.new.boruta.over.ssom.normalizedX, "scaled:scale"))
train.data.new.boruta.over.ssom.normalizedY <- train.data.new.boruta.over[,35]
Y.over <- test.data.new.boruta[,35]
test.data.new.boruta.targetzero <- test.data.new.boruta
test.data.new.boruta.targetzero[,35] <- 0
test.data.new.boruta.ssom.normalizedXY <- list(independent = test.data.new.boruta.ssom.normalizedX, dependent = test.data.new.boruta.targetzero[,35])
# Classification and Prediction Model
set.seed(333)
map.over.ssom <- xyf(train.data.new.boruta.over.ssom.normalizedX, 
                      classvec2classmat(factor(train.data.new.boruta.over.ssom.normalizedY)),
                      grid = somgrid(5,5,"hexagonal"),
                      rlen = 100)
# matrix 1 is based on data from independent variables
# matrix 2 is based on data from dependet variable
plot(map.over.ssom, type = 'changes')
plot(map.over.ssom)
plot(map.over.ssom, type = 'count')

# Prediction
# prediction are going to be based training data
pred.over <- predict(map.over.ssom)
pred.over

# To see how good the model is we need to do predictions based on test data
pred.test.over <- predict(map.over.ssom, newdata = test.data.new.boruta.ssom.normalizedXY)
pred.test.over.cm <- table(Predicted = pred.test.over$predictions[[2]], Actual = Y.over)

pred.test.over.eval <- compute.eval.metrics(pred.test.over.cm)
pred.test.over.eval

# Cluster Boundaries
par(mfrow = c(1,2))
# first plot is for independant variables, second is for dependant variables
plot(map.over.ssom, type = 'codes', main = c("Codes X", "Codes Y"))
map.over.ssom.hc <- cutree(hclust(dist(map.over.ssom$codes[[2]])), 2)
add.cluster.boundaries(map.over.ssom, map.over.ssom.hc)



#######################################
### Summary of all SOM performances ###
######################################

library(ggrepel)
library(ggplot2)
library(plyr)

sensitivity <- c(0.36134454, 0.50840336, 0.43697479,0.05462185,0.7436975,0.7137255,0.08403361,0.6134454,0.5378151)
percision <- c(0.05931034, 0.06167176, 0.05711148,0.20967742,0.6276596,0.5484288,0.09852217,0.6697248,0.5436014)
model<- c("SOM","SOM_U","SOM_O","KNN","KNN_U","KNN_O","SVM","SVM_U","SVM_O")

df <- data.frame(col1 = sensitivity, col2 = percision , col3 = model)

ggplot(df, aes(x=sensitivity, y=percision , color = model , label = model )) + 
  # geom_point(aes(size=17.5))+
  geom_point() +geom_label_repel(aes(label=model),hjust=0, vjust=0)
