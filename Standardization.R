##########################
### Unbalanced train ###
##########################

apply(X = train.data.new.boruta[,-35], MARGIN = 2, FUN = function(x) quantile(x, 0.75))
# Variables with 3rd quantile = 0
# Income.more.then.123K. ; Cont.boat.pol ; Number.of.boat.pol ; Other.contribution ; Number.of.other.policies

apply(X = train.data.new.boruta[,-35], MARGIN = 2, FUN = function(x) length(boxplot.stats(x)$out))

train.data.new.boruta.standardized1 <- apply(X = train.data.new.boruta[,-c(28,32,34,35,36,37)],
                                             MARGIN = 2,
                                             FUN = function(x)
                                               scale(x, center = median(x), scale = IQR(x)))
summary(train.data.new.boruta.standardized1)

normalize.feature <- function( feature ) {
  if ( sum(feature, na.rm = T) == 0 ) feature
  else ((feature - min(feature, na.rm = T))/(max(feature, na.rm = T) -
                                               min(feature, na.rm = T)))
}

train.data.new.boruta.standardized2 <- as.data.frame(apply(train.data.new.boruta[,c(28,32,34,36,37)],
                                                           2, normalize.feature))
summary(train.data.new.boruta.standardized2)

train.data.new.boruta.standardized <- cbind(train.data.new.boruta.standardized1, train.data.new.boruta.standardized2)
train.data.new.boruta.standardized$Num.of.mobile.home.policies <- train.data.new.boruta$Num.of.mobile.home.policies
View(train.data.new.boruta.standardized)

train.data.new.boruta.standardized <- train.data.new.boruta.standardized[,names(train.data.new.boruta)]
summary(train.data.new.boruta.standardized)

############################
### Undersampled train ###
############################

apply(X = train.data.new.boruta.under[,-35], MARGIN = 2, FUN = function(x) quantile(x, 0.75))
# Variables with 3rd quantile = 0
# Income.more.then.123K. ; Cont.boat.pol ; Number.of.boat.pol ; Other.contribution ; Number.of.other.policies, Farmer

apply(X = train.data.new.boruta.under[,-35], MARGIN = 2, FUN = function(x) length(boxplot.stats(x)$out))

train.data.new.boruta.under.standardized1 <- apply(X = train.data.new.boruta.under[,-c(3,13,28,32,34,35,36,37)],
                                             MARGIN = 2,
                                             FUN = function(x)
                                               scale(x, center = median(x), scale = IQR(x)))
summary(train.data.new.boruta.under.standardized1)

normalize.feature <- function( feature ) {
  if ( sum(feature, na.rm = T) == 0 ) feature
  else ((feature - min(feature, na.rm = T))/(max(feature, na.rm = T) -
                                               min(feature, na.rm = T)))
}

train.data.new.boruta.under.standardized2 <- as.data.frame(apply(train.data.new.boruta.under[,c(3,13,28,32,34,36,37)],
                                                           2, normalize.feature))
summary(train.data.new.boruta.under.standardized2)

train.data.new.boruta.under.standardized <- cbind(train.data.new.boruta.under.standardized1, train.data.new.boruta.under.standardized2)
train.data.new.boruta.under.standardized$Num.of.mobile.home.policies <- train.data.new.boruta.under$Num.of.mobile.home.policies
View(train.data.new.boruta.under.standardized)

train.data.new.boruta.under.standardized <- train.data.new.boruta.under.standardized[,names(train.data.new.boruta.under)]
summary(train.data.new.boruta.under.standardized)


############################
### Oversampled train ###
############################

apply(X = train.data.new.boruta.over[,-35], MARGIN = 2, FUN = function(x) quantile(x, 0.75))

apply(X = train.data.new.boruta.over[,-35], MARGIN = 2, FUN = function(x) length(boxplot.stats(x)$out))

train.data.new.boruta.over.standardized1 <- apply(X = train.data.new.boruta.over[,-c(13,28,32,34,35)],
                                                   MARGIN = 2,
                                                   FUN = function(x)
                                                     scale(x, center = median(x), scale = IQR(x)))
summary(train.data.new.boruta.over.standardized1)

normalize.feature <- function( feature ) {
  if ( sum(feature, na.rm = T) == 0 ) feature
  else ((feature - min(feature, na.rm = T))/(max(feature, na.rm = T) -
                                               min(feature, na.rm = T)))
}

train.data.new.boruta.over.standardized2 <- as.data.frame(apply(train.data.new.boruta.over[,c(13,28,32,34)],
                                                                 2, normalize.feature))
summary(train.data.new.boruta.over.standardized2)

train.data.new.boruta.over.standardized <- cbind(train.data.new.boruta.over.standardized1, train.data.new.boruta.over.standardized2)
train.data.new.boruta.over.standardized$Num.of.mobile.home.policies <- train.data.new.boruta.over$Num.of.mobile.home.policies
View(train.data.new.boruta.over.standardized)

train.data.new.boruta.over.standardized <- train.data.new.boruta.over.standardized[,names(train.data.new.boruta.over)]
summary(train.data.new.boruta.over.standardized)


##########################
### Unbalanced test ###
##########################

apply(X = test.data.new.boruta[,-35], MARGIN = 2, FUN = function(x) quantile(x, 0.75))
# Variables with 3rd quantile = 0
# Income.more.then.123K. ; Cont.boat.pol ; Number.of.boat.pol ; Other.contribution ; Number.of.other.policies

apply(X = test.data.new.boruta[,-35], MARGIN = 2, FUN = function(x) length(boxplot.stats(x)$out))

test.data.new.boruta.standardized1 <- apply(X = test.data.new.boruta[,-c(3,28,32,34,35,36,37)],
                                             MARGIN = 2,
                                             FUN = function(x)
                                               scale(x, center = median(x), scale = IQR(x)))
summary(test.data.new.boruta.standardized1)

normalize.feature <- function( feature ) {
  if ( sum(feature, na.rm = T) == 0 ) feature
  else ((feature - min(feature, na.rm = T))/(max(feature, na.rm = T) -
                                               min(feature, na.rm = T)))
}

test.data.new.boruta.standardized2 <- as.data.frame(apply(test.data.new.boruta[,c(3,28,32,34,36,37)],
                                                           2, normalize.feature))
summary(test.data.new.boruta.standardized2)

test.data.new.boruta.standardized <- cbind(test.data.new.boruta.standardized1, test.data.new.boruta.standardized2)
test.data.new.boruta.standardized$Num.of.mobile.home.policies <- test.data.new.boruta$Num.of.mobile.home.policies
View(test.data.new.boruta.standardized)

test.data.new.boruta.standardized <- test.data.new.boruta.standardized[,names(test.data.new.boruta)]
summary(test.data.new.boruta.standardized)


##########################
### Undersampled test ###
##########################


apply(X = test.data.new.boruta.under[,-35], MARGIN = 2, FUN = function(x) quantile(x, 0.75))
# Variables with 3rd quantile = 0
# Income.more.then.123K. ; Cont.boat.pol ; Number.of.boat.pol ; Other.contribution ; Number.of.other.policies, Farmer

apply(X = train.data.new.boruta.under[,-35], MARGIN = 2, FUN = function(x) length(boxplot.stats(x)$out))

test.data.new.boruta.under.standardized1 <- apply(X = test.data.new.boruta.under[,-c(3,13,28,32,34,35,36,37)],
                                                   MARGIN = 2,
                                                   FUN = function(x)
                                                     scale(x, center = median(x), scale = IQR(x)))
summary(train.data.new.boruta.under.standardized1)

normalize.feature <- function( feature ) {
  if ( sum(feature, na.rm = T) == 0 ) feature
  else ((feature - min(feature, na.rm = T))/(max(feature, na.rm = T) -
                                               min(feature, na.rm = T)))
}

test.data.new.boruta.under.standardized2 <- as.data.frame(apply(test.data.new.boruta.under[,c(3,13,28,32,34,36,37)],
                                                                 2, normalize.feature))
summary(test.data.new.boruta.under.standardized2)

test.data.new.boruta.under.standardized <- cbind(test.data.new.boruta.under.standardized1, test.data.new.boruta.under.standardized2)
test.data.new.boruta.under.standardized$Num.of.mobile.home.policies <- test.data.new.boruta.under$Num.of.mobile.home.policies
View(test.data.new.boruta.under.standardized)

test.data.new.boruta.under.standardized <- test.data.new.boruta.under.standardized[,names(test.data.new.boruta.under)]
summary(test.data.new.boruta.under.standardized)


############################
### Oversampled train ###
############################

apply(X = test.data.new.boruta.over[,-35], MARGIN = 2, FUN = function(x) quantile(x, 0.75))

apply(X = test.data.new.boruta.over[,-35], MARGIN = 2, FUN = function(x) length(boxplot.stats(x)$out))

test.data.new.boruta.over.standardized1 <- apply(X = test.data.new.boruta.over[,-c(13,28,32,34,35)],
                                                  MARGIN = 2,
                                                  FUN = function(x)
                                                    scale(x, center = median(x), scale = IQR(x)))
summary(test.data.new.boruta.over.standardized1)

normalize.feature <- function( feature ) {
  if ( sum(feature, na.rm = T) == 0 ) feature
  else ((feature - min(feature, na.rm = T))/(max(feature, na.rm = T) -
                                               min(feature, na.rm = T)))
}

test.data.new.boruta.over.standardized2 <- as.data.frame(apply(test.data.new.boruta.over[,c(13,28,32,34)],
                                                                2, normalize.feature))
summary(test.data.new.boruta.over.standardized2)

test.data.new.boruta.over.standardized <- cbind(test.data.new.boruta.over.standardized1, test.data.new.boruta.over.standardized2)
test.data.new.boruta.over.standardized$Num.of.mobile.home.policies <- test.data.new.boruta.over$Num.of.mobile.home.policies
View(test.data.new.boruta.over.standardized)

test.data.new.boruta.over.standardized <- test.data.new.boruta.over.standardized[,names(test.data.new.boruta.over)]
summary(test.data.new.boruta.over.standardized)
