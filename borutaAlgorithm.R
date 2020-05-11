View(train.data.new)
View(test.data.new)

str(train.data.new)
train.data.new[,1] <- as.integer(train.data.new[,1])
train.data.new[,39] <- as.factor(train.data.new[,39])
train.data.new[,40] <- as.vector(train.data.new[[40]])
train.data.new[,41] <- as.vector(train.data.new[[41]])
class(train.data.new[[41]])

str(test.data.new)
test.data.new[,1] <- as.integer(test.data.new[,1])
test.data.new[,39] <- as.factor(test.data.new[,39])
test.data.new[,40] <- as.vector(test.data.new[[40]])
test.data.new[,41] <- as.vector(test.data.new[[41]])

# Boruta algoritam
library(Boruta)
set.seed(111)
borutaAlgorithm <- Boruta(Num.of.mobile.home.policies ~ ., data = train.data.new, doTrace = 2)
print(borutaAlgorithm)
borutaAlgorithm$finalDecision

# Varijable koje nisu odbacene
nonRejected <- getNonRejectedFormula(borutaAlgorithm)

# Varijable koje su potvrdjene kao znacajne
confirmed <- getConfirmedFormula(borutaAlgorithm)

# las = 2 - nazivi atributa da bufduvertikalni, cex.axis - velicina fonta
plot(borutaAlgorithm, las = 2, cex.axis = 0.7)

plotImpHistory(borutaAlgorithm)

# Popravka tentative atributa - u ovom slucaju nije potrebna
# borutaAlgorithm.without.tentative <- TentativeRoughFix(boruta)
# borutaAlgorithm.without.tentative

# Statistika varijabli kroz iteracije
attStats(borutaAlgorithm)

saveRDS(boruta, file = "borutaAlgorithm.rds")


train.data.new.boruta <- train.data.new
train.data.new.boruta$Number.of.houses <- NULL
train.data.new.boruta$Cont.fire.pol <- NULL
train.data.new.boruta$Number.of.fire.pol <- NULL
train.data.new.boruta$Num.of.car.pol <- NULL

test.data.new.boruta <- test.data.new
test.data.new.boruta$Number.of.houses <- NULL
test.data.new.boruta$Cont.fire.pol <- NULL
test.data.new.boruta$Number.of.fire.pol <- NULL
test.data.new.boruta$Num.of.car.pol <- NULL

write.csv(train.data.new.boruta, 'train.data.new.boruta.csv')
