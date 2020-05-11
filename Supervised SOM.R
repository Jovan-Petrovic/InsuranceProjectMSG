#Normalization
train.data.new.boruta.ssom.normalizedX <- scale(train.data.new.boruta[,-35])
# we will normalized test dataset using mean and sd from train dataset
test.data.new.boruta.ssom.normalizedX <- scale(test.data.new.boruta[,-35], 
                                              center = attr(train.data.new.boruta.ssom.normalized, "scaled:center"),
                                              scale = attr(train.data.new.boruta.ssom.normalized, "scaled:scale"))
train.data.new.boruta.ssom.normalizedY <- train.data.new.boruta[,35]
Y <- test.data.new.boruta[,35]
test.data.new.boruta.targetzero <- test.data.new.boruta
test.data.new.boruta.targetzero[,35] <- 0
test.data.new.boruta.ssom.normalizedXY <- list(independent = test.data.new.boruta.ssom.normalizedX, dependent = test.data.new.boruta.targetzero[,35])

# Classification and Prediction Model
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
table(Predicted = pred.test$predictions[[2]], Actual = Y)

# Cluster Boundaries
par(mfrow = c(1,2))
# first plot is for independant variables, second is for dependant variables
plot(map.ssom, type = 'codes', main = c("Codes X", "Codes Y"))
map.ssom.hc <- cutree(hclust(dist(map.ssom$codes[[2]])), 2)
add.cluster.boundaries(map.ssom, map.ssom.hc)
