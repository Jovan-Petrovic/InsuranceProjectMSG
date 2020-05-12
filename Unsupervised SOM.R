str(train.data.new.boruta)

library(kohonen)
# delete target variable because this is unsupervied method for clustering
train.data.new.boruta.som <- train.data.new.boruta[,-35]
str(train.data.new.boruta.som)
summary(train.data.new.boruta.som)

# normalizing
train.data.new.boruta.som.normalized <- scale(train.data.new.boruta.som)
summary(train.data.new.boruta.som.normalized)

#SOM
#specifing grid
set.seed(111)
g <- somgrid(xdim = 4, ydim = 4, topo = "rectangular")
map <- som(train.data.new.boruta.som.normalized, grid = g, alpha = c(0.05,0.01), radius = 1 )
plot(map, type = 'changes')
plot(map)
map$unit.classif
map$codes
plot(map, type = 'codes', palette.name = rainbow, main = "Mapping of codes")
plot(map, type = 'count')
plot(map, type = 'mapping')
plot(map, type = 'dist.neighbours')
