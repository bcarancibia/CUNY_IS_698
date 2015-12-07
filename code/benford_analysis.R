##############Packages##################
install.packages("benford.analysis")
install.packages("BenfordTests")

require(benford.analysis)
require(BenfordTests)


########################################

####Data

df <- read.csv("/users/bcarancibia/transaction.csv", header=TRUE)

colSums(is.na(df))
isna <- 107347 + 77068 + 1830 + 352188 + 207004 + 106 + 129869 + 471395 + 414617+471+395 + 100 + 421506+398960+339
total <- 471395*74

vars <- c("transaction.type", "default.currency", "transaction.value", "transaction_value_value.date", "transaction_provider.org", 
          "transaction_receiver.org", "reporting.org", "title", "description","start.planned", "end.planned", "start.actual", "end.actual",
          "recipient.country","sector.vocabulary")

data <- df[vars]


EUR <- subset(data, data$default.currency=="EUR")
USD <- subset(data, data$default.currency=="USD")
XDR <- subset(data, data$default.currency=="XDR")
AUD <- subset(data, data$default.currency=="AUD")
CAD <- subset(data, data$default.currency=="CAD")
CHF <- subset(data, data$default.currency=="CHF")
DKK <- subset(data, data$default.currency=="DKK")
GBP <- subset(data, data$default.currency=="GBP")
JPY <- subset(data, data$default.currency=="JPY")
NZD <- subset(data, data$default.currency=="NZD")
NOK <- subset(data, data$default.currency=="NOK")
XBT <- subset(data, data$default.currency=="XBT")
UGX <- subset(data, data$default.currency=="UGX")
ETB <- subset(data, data$default.currency=="ETB")
ZMK <- subset(data, data$default.currency=="ZMK")
SLL <- subset(data, data$default.currency=="SLL")
ZAR <- subset(data, data$default.currency=="ZAR")
GHS <- subset(data, data$default.currency=="GHS")
INR <- subset(data, data$default.currency=="INR")
blank <- subset(data, data$default.currency=="")


EUR$usd.conversion <- (EUR$transaction.value*1.06)
USD$usd.conversion <- (USD$transaction.value)
XDR$usd.conversion <- (XDR$transaction.value*1.38)
AUD$usd.conversion <- (AUD$transaction.value*0.72)
CAD$usd.conversion <- (CAD$transaction.value*0.75)
CHF$usd.conversion <- (CHF$transaction.value*0.98)
DKK$usd.conversion <- (DKK$transaction.value*0.14)
GBP$usd.conversion <- (GBP$transaction.value*1.58)
JPY$usd.conversion <- (JPY$transaction.value*0.0081)
NZD$usd.conversion <- (NZD$transaction.value*0.66)
NOK$usd.conversion <- (NOK$transaction.value*0.12)
XBT$usd.conversion <- (XBT$transaction.value*0.94)
UGX$usd.conversion <- (UGX$transaction.value*0.00030)
ETB$usd.conversion <- (ETB$transaction.value*0.047)
ZMK$usd.conversion <- (ZMK$transaction.value*0.00019)
SLL$usd.conversion <- (SLL$transaction.value*0.00023)
ZAR$usd.conversion <- (ZAR$transaction.value*0.072)
GHS$usd.conversion <- (GHS$transaction.value*0.26)
INR$usd.conversion <- (INR$transaction.value*0.015)
blank$usd.conversion <- (blank$transaction.value)

clean.data <- rbind(EUR, USD, XDR, AUD, CAD, CHF, DKK, GBP, JPY, NZD, NOK, XBT, UGX, ETB, ZMK, SLL, ZAR, GHS, INR, blank)

benfords <- subset(clean.data, clean.data$usd.conversion!="")

####Summary Statistics

unique(benfords$transaction_provider.org)
unique(benfords$transaction_receiver.org)
unique(benfords$reporting.org)
unique(benfords$recipient.country)

summary(benfords$usd.conversion)

#Benford converted
benford.data <- benford(benfords$usd.conversion, number.of.digits = 2, sign="both")

plot(benford.data)

print(benford.data)

head(suspectsTable(benford.data), 10)

head(duplicatesTable(benford.data), 10)

suspects <- getSuspects(benford.data, benfords, how.many=2)
suspects

country.count <- as.data.frame(table(suspects$recipient.country))
country.count <- country.count[order(country.count$Freq,decreasing=TRUE),]

org.count <- as.data.frame(table(suspects$transaction_provider.org))
org.count <- org.count[order(org.count$Freq,decreasing=TRUE),]

duplicates <- getDuplicates(benford.data, benfords,how.many=2)
duplicates

########take data and remove the blanks from country and organizations

benfords2 <- subset(benfords, benfords$recipient.country!="" | benfords$recipient.country!=" " )
benfords2 <- subset(benfords2, benfords$transaction_provider.org!="")

benford2.data <- benford(benfords2$usd.conversion, number.of.digits = 3, sign="both")

plot(benford2.data)

print(benford2.data)

head(suspectsTable(benford2.data), 10)

head(duplicatesTable(benford2.data), 10)

suspects <- getSuspects(benford2.data, benfords2, how.many=2)
suspects <- subset(suspects, suspects$recipient.country!="")

country.count <- as.data.frame(table(suspects$recipient.country))
country.count <- country.count[order(country.count$Freq,decreasing=TRUE),]

org.count <- as.data.frame(table(suspects$transaction_provider.org))
org.count <- org.count[order(org.count$Freq,decreasing=TRUE),]

duplicates <- getDuplicates(benford.data, benfords,how.many=2)
duplicates






