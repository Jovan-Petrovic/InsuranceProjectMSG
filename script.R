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