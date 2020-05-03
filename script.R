#Reding data from csv files
train.data <- read.csv("ticdata2000.csv.xls",header = F,  stringsAsFactors = FALSE)
train.data[1,1] <- 33
colnames(train.data) <- c("Customer.Subtype", "Number.of.houses", "Avg.size.household","Avg.age","Customer.main.type","Roman.catholic","Protestant","Other.religion", "No.religion", "Married", "Living.together", "Other.relation","Singles", "Household.without.children","Household.with.children", "High.level.education", "Medium.level.education", "Lower.level.education", "High.status", "Entrepreneur","Farmer","Middle.management","Skilled.labourers", "Unskilled.labourers", "Social.class.A","Social.class.B1","Social.class.B2",  "Social.class.C", "Social.class.D",  "Rented.house", "Home.owners", "1.car", "2.cars", "No.car","National.Health.Service","Private.health.insurance","Income.less.then.30K","Income.in.30.45K", "Income.in.45.75K", "Income.in.75.122K", "Income.more.then.123K", "Average.income", "Purchasing.power.class", "Cont.private.third.party.insr", "Cont.third.party.insr.firms", "Cont.third.party.insr.agriculture", "Cont.car.pol", "Cont.delivery.van.pol", "Cont.motorcycle.scooter.pol", "Cont.lorry.pol", "Cont.trailer.pol", "Cont.tractor.pol", "Cont.agricultural.machines.pol", "Cont.moped.pol", "Cont.life.insr", "Cont.private.accident.insr.pol", "Cont.family.accidents.insr.pol", "Cont.disability.insr.pol", "Cont.fire.pol", "Cont.surfboard.pol","Cont.boat.pol",
                          "Cont.bicycle.pol",
                          "Cont.property.insr.pol",
                          "Cont.social.security.insr.pol",
                          "Num.of.private.third.party.insur",
                          "Num.of.third.party.insr.firms",
                          "Num.of.third.party.insr.agriculture",
                          "Num.of.car.pol",
                          "Num.of.delivery.van.pol",
                          "Num.of.motorcycle.scooter.pol",
                          "Num.of.lorry.pol",
                          "Numb.of.trailer.pol",
                          "Num.of.tractor.pol",
                          "Num.of.agricultural.machines.pol",
                          "Num.of.moped.pol",
                          "Number.of.life.insr",
                          "Number.of.private.accident.insr.pol",
                          "Number.of.family.accidents.insr.pol",
                          "Number.of.disability.insr.pol",
                          "Number.of.fire.pol","Num.of.surfboard.pol", "Number.of.boat.pol","Num.of.bicycle.pol", "Num.of.property.insr.pol", "Num.of.social.security.insr.pol", "Num.of.mobile.home.policies")

# train data
View(train.data)
# Reading test data
test.data <- read.csv("ticeval2000.csv.xls",header = F,  stringsAsFactors = FALSE)
test.data[1,1] <- 33

colnames(test.data) <- colnames(train.data[-86])

# Reading target for test data
test.target <- read.csv("tictarget.csv",header = F,  stringsAsFactors = FALSE)
colnames(test.target) <- colnames(train.data[86])

test.data <- cbind(test.data,test.target)

#correlation
install.packages("corrplot")
library(corrplot)
corr_matrix<- cor(train.data[,c(16:29,37:42,86)])
corrplot(corr_matrix, type = "upper", diag = F,tl.pos = NULL, tl.cex = 0.3, tl.col = "red", tl.offset = 0.4, tl.srt = 90)



data <- rbind(train.data,test.data)
corr_matrix_2<- cor(data[,c(16:29,37:42)])
corrplot(corr_matrix_2, type = "upper", diag = F,tl.pos = NULL, tl.cex = 0.3, tl.col = "red", tl.offset = 0.4, tl.srt = 90)



zeros_train <- colSums(train.data[,c(44:85)]==0)/nrow(train.data[,c(44:85)]==0)*100
zeros_sum <- colSums(data[,c(44:85)]==0)/nrow(data[,c(44:85)]==0)*100
zeros_train
zeros_sum
which(zeros_train<90)
which(zeros_sum<90)



corr_matrix_2<- cor(train.data[,c(44:64,86)])
corrplot(corr_matrix_2, type = "upper", diag = F,tl.pos = NULL, tl.cex = 0.3, tl.col = "red", tl.offset = 0.4, tl.srt = 90)
#izbaciti number of..
#cont. car, fire, private third party...

corr_matrix_3<- cor(data[,c(16:18,25:29,37:41,86)])
corrplot(corr_matrix_3, type = "upper", diag = F,tl.pos = NULL, tl.cex = 0.3, tl.col = "red", tl.offset = 0.4, tl.srt = 90)


#Izbacivanje kolona nakon konsultovanja sa profesorkom 

#ostaju
colnames(train.data[c(44, 47, 59, 61)])

#Dobijanje agregirane varijable za uloženi novac u ostala osiguranja
train.data.other.contribution <- train.data[, 44:64]
train.data.other.contribution$Cont.private.third.party.insr <- NULL
train.data.other.contribution$Cont.car.pol <- NULL
train.data.other.contribution$Cont.fire.pol <- NULL
train.data.other.contribution$Cont.boat.pol <- NULL
train.data.other.contribution <- as.data.frame(apply(train.data.other.contribution, 1, sum))

#Dobijanje agregirane varijable za broj kupljenih ostalih osiguranja
train.data.other.number.of.pol <- train.data[, 65:85]
train.data.other.number.of.pol$Num.of.private.third.party.insur <- NULL
train.data.other.number.of.pol$Num.of.car.pol <- NULL
train.data.other.number.of.pol$Number.of.fire.pol <- NULL
train.data.other.number.of.pol$Number.of.boat.pol <- NULL
train.data.other.number.of.pol <- as.data.frame(apply(train.data.other.number.of.pol, 1, sum))

train.data.new <- train.data[,-c(6:9, 42, 16:18, 25:29, 45, 46, 48:58, 60, 62:64, 66, 67, 69:79, 81, 83:85)]
train.data.new$Other.contribution <- train.data.other.contribution
train.data.new$Number.of.other.policies <- train.data.other.number.of.pol

#Za data set za testiranje dobijanje agregirane varijable za uloženi novac u ostala osiguranja
test.data.other.contribution <- test.data[, 44:64]
test.data.other.contribution$Cont.private.third.party.insr <- NULL
test.data.other.contribution$Cont.car.pol <- NULL
test.data.other.contribution$Cont.fire.pol <- NULL
test.data.other.contribution$Cont.boat.pol <- NULL
test.data.other.contribution <- as.data.frame(apply(test.data.other.contribution, 1, sum))

#Za data set za testiranje dobijanje agregirane varijable za broj kupljenih ostalih osiguranja
test.data.other.number.of.pol <- test.data[, 65:85]
test.data.other.number.of.pol$Num.of.private.third.party.insur <- NULL
test.data.other.number.of.pol$Num.of.car.pol <- NULL
test.data.other.number.of.pol$Number.of.fire.pol <- NULL
test.data.other.number.of.pol$Number.of.boat.pol <- NULL
test.data.other.number.of.pol <- as.data.frame(apply(test.data.other.number.of.pol, 1, sum))

test.data.new <- test.data[,-c(6:9, 42, 16:18, 25:29, 45, 46, 48:58, 60, 62:64, 66, 67, 69:79, 81, 83:85)]
test.data.new$Other.contribution <- test.data.other.contribution
test.data.new$Number.of.other.policies <- test.data.other.number.of.pol


# Unbalanced structure of target variable
table(train.data$Num.of.mobile.home.policies)
plot(table(train.data$Num.of.mobile.home.policies), type = "h", col = "red", lwd = 15, xlab = "Da li je korisnik kupio osiguranje: 0 - Ne, 1 - Da", ylab = "Broj korisnika", main = "Prikaz nebalansirane strukture izlazne promenljive")

# All datasets
# str(train.data)
# str(test.data)
# str(data)
# str(train.data.new)
# str(test.data.new)
