###########################
### Undersampling train ###
###########################

# Taking all observations with dependant variable = 1
train.data.new.boruta.under <- train.data.new.boruta[train.data.new.boruta$Num.of.mobile.home.policies==1,]
# Randomly select observations with dependent variable = 0
zero.observations <- train.data.new.boruta[train.data.new.boruta$Num.of.mobile.home.policies==0,]
zero.observations.under <- zero.observations[sample(nrow(zero.observations), length(train.data.new.boruta.under$Num.of.mobile.home.policies)),]
#Appending rows of randomly selected 0s in our undersampled data frame
train.data.new.boruta.under <- rbind(train.data.new.boruta.under, zero.observations.under)

#Let's verify that number of 1s and 0s in our undersampled data are equal
undersampled.frequency.num.of.mobile.home.pol <- data.frame(
  Number.of.mobile.home.policies = levels(train.data.new.boruta.under$Num.of.mobile.home.policies),
  Count = as.numeric(table(train.data.new.boruta.under$Num.of.mobile.home.policies)))
undersampled.frequency.num.of.mobile.home.pol

# let's random shuffle rows
View(train.data.new.boruta.under)
str(train.data.new.boruta.under)
set.seed(111)
rows <- sample(nrow(train.data.new.boruta.under))
rows
train.data.new.boruta.under <- train.data.new.boruta.under[rows, ]


#########################
### Oversampling train ###
#########################


# First let's recall the ratio of 1s and 0s in our original training dataset
paste0("Ratio of 1s to 0s- 1:",
       as.numeric(table(train.data.new.boruta$Num.of.mobile.home.policies))[1]/
         as.numeric(table(train.data.new.boruta$Num.of.mobile.home.policies))[2])

# Now let's duplicate the observations with dependent variable = 1 to make the ratio approximately 1:1
train.data.new.boruta.over.ones <- train.data.new.boruta[train.data.new.boruta$Num.of.mobile.home.policies==1,]
train.data.new.boruta.over <- NULL
for (i in 1:15){
  train.data.new.boruta.over <- rbind(train.data.new.boruta.over, train.data.new.boruta.over.ones)
}
train.data.new.boruta.over <- rbind(train.data.new.boruta.over, train.data.new.boruta[train.data.new.boruta$Num.of.mobile.home.policies==0,])

#Let's verify the number of 1s and 0s in our oversampled data
oversampled.frequency.num.of.mobile.home.pol <- data.frame(
  Number.of.mobile.home.policies = levels(train.data.new.boruta.over$Num.of.mobile.home.policies),
  Count = as.numeric(table(train.data.new.boruta.over$Num.of.mobile.home.policies)))
oversampled.frequency.num.of.mobile.home.pol

# let's random shuffle rows
View(train.data.new.boruta.over)
str(train.data.new.boruta.over)
set.seed(111)
rows <- sample(nrow(train.data.new.boruta.over))
rows
train.data.new.boruta.over <- train.data.new.boruta.over[rows, ]


##########################
### Undersampling test ###
##########################

# Taking all observations with dependant variable = 1
test.data.new.boruta.under <- test.data.new.boruta[test.data.new.boruta$Num.of.mobile.home.policies==1,]
# Randomly select observations with dependent variable = 0
zero.observations <- test.data.new.boruta[test.data.new.boruta$Num.of.mobile.home.policies==0,]
zero.observations.under <- zero.observations[sample(nrow(zero.observations), length(test.data.new.boruta.under$Num.of.mobile.home.policies)),]
#Appending rows of randomly selected 0s in our undersampled data frame
test.data.new.boruta.under <- rbind(test.data.new.boruta.under, zero.observations.under)

#Let's verify that number of 1s and 0s in our undersampled data are equal
undersampled.frequency.num.of.mobile.home.pol <- data.frame(
  Number.of.mobile.home.policies = levels(test.data.new.boruta.under$Num.of.mobile.home.policies),
  Count = as.numeric(table(test.data.new.boruta.under$Num.of.mobile.home.policies)))
undersampled.frequency.num.of.mobile.home.pol

# let's random shuffle rows
View(test.data.new.boruta.under)
str(test.data.new.boruta.under)
set.seed(111)
rows <- sample(nrow(test.data.new.boruta.under))
rows
test.data.new.boruta.under <- test.data.new.boruta.under[rows, ]



#########################
### Oversampling test ###
#########################


# First let's recall the ratio of 1s and 0s in our original training dataset
paste0("Ratio of 1s to 0s- 1:",
       as.numeric(table(test.data.new.boruta$Num.of.mobile.home.policies))[1]/
         as.numeric(table(test.data.new.boruta$Num.of.mobile.home.policies))[2])

# Now let's duplicate the observations with dependent variable = 1 to make the ratio approximately 1:1
test.data.new.boruta.over.ones <- test.data.new.boruta[test.data.new.boruta$Num.of.mobile.home.policies==1,]
test.data.new.boruta.over <- NULL
for (i in 1:15){
  test.data.new.boruta.over <- rbind(test.data.new.boruta.over, test.data.new.boruta.over.ones)
}
test.data.new.boruta.over <- rbind(test.data.new.boruta.over, test.data.new.boruta[test.data.new.boruta$Num.of.mobile.home.policies==0,])

#Let's verify the number of 1s and 0s in our oversampled data
oversampled.frequency.num.of.mobile.home.pol <- data.frame(
  Number.of.mobile.home.policies = levels(test.data.new.boruta.over$Num.of.mobile.home.policies),
  Count = as.numeric(table(test.data.new.boruta.over$Num.of.mobile.home.policies)))
oversampled.frequency.num.of.mobile.home.pol

# let's random shuffle rows
View(test.data.new.boruta.over)
str(test.data.new.boruta.over)
set.seed(111)
rows <- sample(nrow(test.data.new.boruta.over))
rows
test.data.new.boruta.over <- test.data.new.boruta.over[rows, ]
