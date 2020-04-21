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
