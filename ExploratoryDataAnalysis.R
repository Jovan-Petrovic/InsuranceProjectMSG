View(data)

# High level overview of data
str(data)

# Convert first and fifth variable to character and last to factor
data[,1] <- as.integer(data[,1])
data[,5] <- as.integer(data[,5])
data[,86] <- as.factor(data[,86])

summary(data)

class(data)

#first six variables
head(data)
# last six variables
tail(data)

### UNIVARIATE ANALYSIS ###
# Univariate data - samples of one variable
# Univariate data analysis isn't concerned with the 'why'. It is just to describe the data as is.

# Two key things to discover in EDA: Central tendency and spread
# Types of graphs to use: box plot, histogram, density plot, pie graph

#central tendency
summary(data$Married)
boxplot(data$Married)

#spread
hist(data$Married)
plot(density(data$Married), main = "Married density spread")
plot(table(data$Married))


# Target variable graphs
plot(data$Num.of.mobile.home.policies, main = "0 - Not bought, 1 - Bought")
pie(table(data$Num.of.mobile.home.policies), main = "0 - Not bought, 1 - Bought")

# Home owners
summary(data$Rented.house)
plot(data$Rented.house)

### Multivariate data analysis ###
# Relationship between multiple variables

# Categorical vs continuous variable
by(data$Married, data$Num.of.mobile.home.policies, summary)
by(data$Married, data$Num.of.mobile.home.policies, median)

boxplot(data$Married~data$Num.of.mobile.home.policies, notch = T, col = c("grey","gold"), main = "Distribution of married users among those who bought caravan insurance")

# Those who are married and bought caravan insurance
library("dplyr")
bought = filter(data, Num.of.mobile.home.policies == "1")
View(bought)
plot(table(bought$Married), main = "Num of married users who bought caravan insurance")

# Those who bought car insurance policie and caravan insurance policie
barplot(table(data$Num.of.car.pol[data$Num.of.mobile.home.policies==1]),border="dark blue",main = "PURCHASE OF CARAVAN POLICY vs NUMBER OF CAR POLICIES",xlab = "Number of car policies",ylab = "Number of customers")


colors=c("blue","red","green","grey")
col=colors
pie(table(data$Num.of.car.pol[data$Num.of.mobile.home.policies==1]),main ="PURCHASE OF CARAVAN POLICY vs NUMBER OF CAR POLICIES",col=colors)
box()


barplot(table(data$Customer.Subtype[data$Num.of.mobile.home.policies==1]), border="dark blue",main = "PURCHASE OF CARAVAN POLICY vs CUSTOMER SUBTYPE",xlab="Customer subtype",ylab="Number of customers")

# Check why names cannot be changed
# names(table(data$Avg.age[data$Num.of.mobile.home.policies==1]))=c("20 to 30","30 to 40","40 to 50","50 to 60","60 to 70","70 to 80")
barplot(table(data$Avg.age[data$Num.of.mobile.home.policies==1]), col=rainbow(6),main = "PURCHASE OF CARAVAN POLICY vs AVE AGE",xlab="Avg age or Age group",ylab="Number of customers")


library(sm)
sm.density.compare(data$Married,data$Num.of.mobile.home.policies, xlab = "Married")
education_legend = factor(data$Num.of.mobile.home.policies, labels = c("Not bought", "Bought"))
colfill <- c(2:(2+length(levels(education_legend))))
legend(locator(1), levels(education_legend), fill = colfill)

xtabs(~data$Avg.age+data$Num.of.mobile.home.policies, data)
plot(xtabs(~data$Avg.age+data$Num.of.mobile.home.policies, data), main = "Average age and Caravan insurance")

xtabs(~data$Income.less.then.30K+data$Num.of.mobile.home.policies, data)
plot(xtabs(~data$Income.less.then.30K+data$Num.of.mobile.home.policies, data), main = "Income less than 30K and Caravan insurance")


library(gmodels)
#chisq - level of significance 
CrossTable(data$Avg.age, data$Num.of.mobile.home.policies, chisq = TRUE, prop.t = F)
# use chi-sq to find p-value wich will show significance
# p-value > 0.05 is not significant

CrossTable(data$Married, data$Num.of.mobile.home.policies, chisq = TRUE, prop.t = F)

# For continuous vs continuous
scatter.smooth(data$Married, data$Home.owners)


##############################################################
### UNIVARIATE ANALYSIS ###

# first category - socio-demografic variables
# almost all variables are in max range 0-9
apply(data[6:43], 2, summary)
apply(data[6:43], 2, sd)

# second category - contributon variables
uni.eda.cont <- apply(data[44:64], 2, summary)
apply(data[44:64], 2, sd)
# uni.eda.cont is matrix
class(uni.eda.cont)
# transforming to data.frame
data.uni.eda.cont <- as.data.frame(uni.eda.cont)
View(data.uni.eda.cont)
# only 3 attributes have at least one opservation with maximum contribution
sum(data.uni.eda.cont["Max.",]==9)
# only 2 attributes have median different than zero
sum(data.uni.eda.cont["Median",]!=0)
# only 3 attributes have 3rd quartile different than zero
sum(data.uni.eda.cont["3rd Qu.",]!=0)

# third category - number of variables
uni.eda.num.of <- apply(data[65:85], 2, summary)
View(uni.eda.num.of)
apply(data[65:85], 2, sd)
class(uni.eda.num.of)
data.uni.eda.num.of <- as.data.frame(uni.eda.num.of)
View(data.uni.eda.num.of)
# only 1 attribute have at least one opservation with maximum number of policies of 12
sum(data.uni.eda.num.of["Max.",]==12)
# 4 attribute have no opservation with number of policies greater than 1
sum(data.uni.eda.num.of["Max.",]==1)
# only 2 attributes have median different than zero
sum(data.uni.eda.num.of["Median",]!=0)
# only 3 attributes have 3rd quartile different than zero
sum(data.uni.eda.num.of["3rd Qu.",]!=0)


##############################################################
### MULTIVARIATE ANALYSIS ###

by(data$Cont.car.pol, data$Num.of.mobile.home.policies, summary)
by(data$Cont.fire.pol, data$Num.of.mobile.home.policies, summary)

boxplot(data$Cont.car.pol~data$Num.of.mobile.home.policies, col = c("grey","gold"),
        main = "Ulaganje u osiguranje automobila u odnosu na izlaznu promenljivu",
        xlab = "Da li je korisnik kupio osiguranje karavana",
        ylab = "Visina ulaganja u osiguranje automobila")
boxplot(data$Cont.fire.pol~data$Num.of.mobile.home.policies,
        col = c("grey","gold"), main = "Ulaganje u osiguranje od požara u odnosu na izlaznu promenljivu",
        xlab = "Da li je korisnik kupio osiguranje karavana",
        ylab = "Visina ulaganja u osiguranje od požara")

library("dplyr")
bought = filter(data, Num.of.mobile.home.policies == "1")
View(bought)
plot(table(bought$Cont.car.pol), main = "Num of car users who bought caravan insurance")

barplot(table(data$Cont.car.pol[data$Num.of.mobile.home.policies==1]),border="dark blue",main = "Ulaganje u osiguranje automobila korisnika koji su kupili osiguranje karavana",xlab = "Ulaganje u osiguranje automobila",ylab = "Broj korisnika")
barplot(table(data$Cont.fire.pol[data$Num.of.mobile.home.policies==1]),border="dark blue",main = "PURCHASE OF CARAVAN POLICY vs NUMBER OF CAR POLICIES",xlab = "Number of car policies",ylab = "Number of customers")

library(gmodels)
#chisq - level of significance 
CrossTable(data$Cont.car.pol, data$Num.of.mobile.home.policies, chisq = TRUE, prop.t = F)
# use chi-sq to find p-value wich will show significance
# p-value > 0.05 is not significant

CrossTable(data$Cont.fire.pol, data$Num.of.mobile.home.policies, chisq = TRUE, prop.t = F)
