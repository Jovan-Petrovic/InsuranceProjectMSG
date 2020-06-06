str(train.data.new.boruta)

library(kohonen)
# delete target variable because this is unsupervied method for clustering
train.data.new.boruta.som <- train.data.new.boruta[,-35]
str(train.data.new.boruta.som)
summary(train.data.new.boruta.som)

# check outliers
apply(X = train.data.new.boruta.som[,], MARGIN = 2, FUN = function(x) length(boxplot.stats(x)$out))

# check the distribution of all variables
apply(X = train.data.new.boruta.som[1:5000,], MARGIN = 2, FUN = shapiro.test)
# none of variables are normaly distributed, so we'll standardize with median and IQR

# Standardization
train.data.new.boruta.som.standardized <- scale(train.data.new.boruta.som)
"train.data.new.boruta.som.standardized <- apply(X = train.data.new.boruta.som,
                                                MARGIN = 2,
                                                FUN = function(x) scale(x, center = median(x), scale = IQR(x)))
train.data.new.boruta.som.standardized <- as.data.frame(train.data.new.boruta.som.standardized) "
summary(train.data.new.boruta.som.standardized)

#SOM
#specifing grid
set.seed(111)
g <- somgrid(xdim = 4, ydim = 4, topo = "rectangular")
map <- som(train.data.new.boruta.som.standardized, grid = g, alpha = c(0.05,0.01), radius = 1 )
plot(map, type = 'changes')
plot(map)
map$unit.classif
map$codes
plot(map, type = 'codes', palette.name = rainbow, main = "Mapping of codes")
plot(map, type = 'count')
plot(map, type = 'mapping')
plot(map, type = 'dist.neighbours')



######################################
### SOM only with income variables ###
######################################

train.data.new.boruta.income.som <- train.data.new.boruta[,c('Income.less.then.30K','Income.in.30.45K','Income.in.45.75K','Income.in.75.122K','Income.more.then.123K')]
str(train.data.new.boruta.income.som)
summary(train.data.new.boruta.income.som)
# since all ranges are equal standardization is not neccessary

library(kohonen)

train.data.new.boruta.income.som.matrix <- as.matrix(train.data.new.boruta.income.som)

#SOM
#specifing grid
set.seed(111)
g <- somgrid(xdim = 4, ydim = 4, topo = "rectangular")
map <- som(train.data.new.boruta.income.som.matrix, grid = g, alpha = c(0.05,0.01),
           radius = 1, keep.data = TRUE, dist.fcts = "euclidean") 


plot(map, type = 'codes', palette.name = rainbow, main = "Mapping of codes")
plot(map, type = 'mapping')

map$unit.classif
train.data.new.boruta.income.som[,6] <- map$unit.classif

head(train.data.new.boruta.income.som)

# Open a bigger device window explicitly
dev.new(width=10, height=10)
plot(map, type = 'changes')

plot(map)
map$codes

plot(map, type = 'counts')

plot(map, type = 'dist.neighbours')

heatmap.som <- function(model){
  for (i in 1:5) {
    plot(model, type = "property", property = getCodes(model)[,i], 
         main = colnames(getCodes(model))[i]) 
  }
}
par(mfrow=c(3,2))
heatmap.som(map)
par(mfrow=c(1,1))

# clustering
library(factoextra)
set.seed(111)
fviz_nbclust(map$codes[[1]], kmeans, method = "wss")

set.seed(111)
clust <- kmeans(map$codes[[1]], 3)

plot(map, type = "codes", bgcol = rainbow(9)[clust$cluster], main = "Cluster Map")
add.cluster.boundaries(map, clust$cluster)

# know cluster each data
train.data.new.boruta.income.som[,6] <- NULL
ads.cluster <- data.frame(train.data.new.boruta.income.som, Cluster = clust$cluster[map$unit.classif])
View(ads.cluster)
