### Undersampling ###

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



### Oversampling ###


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