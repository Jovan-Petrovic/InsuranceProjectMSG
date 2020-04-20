library(Boruta)
library(mlbench)
library(caret)
library(randomForest)

# konvertovanje prve varijable train seta u integer i poslednje u factor
View(train.data)
str(train.data)
train.data[,1] <- as.integer(train.data[,1])
train.data[,86] <- as.factor(train.data[,86])

# konvertovanje prve varijable test seta u integer i poslednje u factor
View(test.data)
str(test.data)
test.data[,1] <- as.integer(test.data[,1])
test.data[,86] <- as.factor(test.data[,86])

# Boruta algoritam
set.seed(111)
boruta <- Boruta(Num.of.mobile.home.policies ~ ., data = train.data, doTrace = 2)
print(boruta)
boruta$finalDecision

# Varijable koje nisu odbacene
nonRejected <- getNonRejectedFormula(boruta)

# Varijable koje su potvrdjene kao znacajne
confirmed <- getConfirmedFormula(boruta)

# las = 2 - nazivi atributa da bufduvertikalni, cex.axis - velicina fonta
plot(boruta, las = 2, cex.axis = 0.7)
plotImpHistory(boruta)

# Popravka tentative atributa
boruta.without.tentative <- TentativeRoughFix(boruta)
boruta.without.tentative

# Statistika varijabli kroz iteracije
attStats(boruta)

# Provera kako oslobadjanje velikog broja atributa utice na preciznost predvidjanja pomcu modela Random Forest
# Ceo Random Forest
set.seed(222)
rf86 <- randomForest(Num.of.mobile.home.policies ~ ., data = train.data)
rf86

# NonRejected Random Forest
rfNonRejected <- randomForest(Num.of.mobile.home.policies ~ Customer.Subtype + Avg.size.household + 
                                Avg.age + Customer.main.type + Protestant + Other.religion + 
                                No.religion + Married + Living.together + Other.relation + 
                                Singles + Household.without.children + Household.with.children + 
                                High.level.education + Medium.level.education + Lower.level.education + 
                                High.status + Entrepreneur + Farmer + Middle.management + 
                                Skilled.labourers + Unskilled.labourers + Social.class.A + 
                                Social.class.B1 + Social.class.B2 + Social.class.C + Social.class.D + 
                                Rented.house + Home.owners + `One.car` + `Two.cars` + 
                                No.car + National.Health.Service + Private.health.insurance + 
                                Income.less.then.30K + Income.in.30.45K + Income.in.45.75K + 
                                Income.in.75.122K + Income.more.then.123K + Average.income + 
                                Purchasing.power.class + Cont.car.pol + Cont.tractor.pol + 
                                Cont.moped.pol + Cont.disability.insr.pol + Cont.fire.pol + 
                                Cont.boat.pol + Cont.social.security.insr.pol + Num.of.moped.pol + 
                                Number.of.boat.pol + Num.of.social.security.insr.pol, data = train.data)

# Confirmed Random Forest
rfConfirmed <- randomForest(Num.of.mobile.home.policies ~ Customer.Subtype + Avg.size.household + 
                              Avg.age + Customer.main.type + Protestant + No.religion + 
                              Married + Other.relation + Singles + Household.without.children + 
                              Household.with.children + High.level.education + Medium.level.education + 
                              Lower.level.education + High.status + Entrepreneur + Farmer + 
                              Middle.management + Skilled.labourers + Unskilled.labourers + 
                              Social.class.A + Social.class.B1 + Social.class.B2 + Social.class.C + 
                              Social.class.D + Rented.house + Home.owners + `One.car` + 
                              No.car + National.Health.Service + Private.health.insurance + 
                              Income.less.then.30K + Income.in.30.45K + Income.in.45.75K + 
                              Income.in.75.122K + Average.income + Purchasing.power.class + 
                              Cont.car.pol + Cont.moped.pol + Cont.fire.pol + Cont.boat.pol + 
                              Num.of.moped.pol + Number.of.boat.pol, data = train.data)

# Predikcija
# Ceo dataset
p <- predict(rf86, test.data)
library(e1071)
confusionMatrix(p, test.data$Num.of.mobile.home.policies)

#NonRejected dataset
pNonRejected <- predict(rfNonRejected, test.data)
confusionMatrix(pNonRejected, test.data$Num.of.mobile.home.policies)

# Confirmed dataset
pConfirmed <- predict(rfConfirmed, test.data)
confusionMatrix(pConfirmed, test.data$Num.of.mobile.home.policies)

saveRDS(boruta, file = "boruta.rds")
