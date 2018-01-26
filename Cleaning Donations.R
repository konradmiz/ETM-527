
library(readr)
library(readxl)
library(lubridate)

install.packages("radiant.data")
library(radiant.data)

donations2013 <- read.csv("C:\\Users\\Konrad\\Desktop\\ETM 527\\Donations\\2013 Bike Donations.csv")
#donations2014 <- read.csv("C:\\Users\\Konrad\\Desktop\\ETM 527\\Donations\\2014 Bike Donations.csv")
#donations2015 <- read.csv("C:\\Users\\Konrad\\Desktop\\ETM 527\\Donations\\2015 Bike Donations.csv")
#donations2016 <- read.csv("C:\\Users\\Konrad\\Desktop\\ETM 527\\Donations\\2016 Bike Donations.csv")
#donations2017 <- read.csv("C:\\Users\\Konrad\\Desktop\\ETM 527\\Donations\\2017 Bike Donations.csv")

summary_2013 <- summary(donations2013)
str(donations2013)

summary_2013

sample_2013 <- donations2013[1:10000, ]
summary(sample_2013)


sample_2013$Date.Recorded <- as.character(sample_2013$Date.Recorded)
split_dates <- strsplit(sample_2013$Date.Recorded, "/")

sample_2013$Date.Recorded.Year <- lapply(split_dates, function(l) l[[3]])
sample_2013$Date.Recorded.Month <- lapply(split_dates, function(l) l[[1]])
sample_2013$Date.Recorded.Day <- lapply(split_dates, function(l) l[[2]])
sample_2013$Date.Recorded.Time <- substr(sample_2013$Date.Recorded.Year, start = 5, stop = 10)
sample_2013$Date.Recorded.Year <- substr(sample_2013$Date.Recorded.Year, start = 1, stop = 4)


firstcol = which(colnames(sample_2013)=="Date.Recorded.Year")
lastcol = which(colnames(sample_2013)=="Date.Recorded.Day")

sample_2013$Date.Recorded.Year <- as.numeric(sample_2013$Date.Recorded.Year)
sample_2013$Date.Recorded.Month <- as.numeric(sample_2013$Date.Recorded.Month)
sample_2013$Date.Recorded.Day <- as.numeric(sample_2013$Date.Recorded.Day)
sample_2013$Date.Recorded.Time <- as.Date(sample_2013$Date.Recorded.Time, format = 'h:m')

summary(sample_2013[, firstcol:lastcol])

summary(sample_2013$Date.Recorded.Time)

summary(sample_2013$Donor.Gender)

str(sample_2013$Donor.Gender)

sample_2013$Donor.Gender <- refactor(sample_2013$Donor.Gender, c("Female", "Male"))

summary(sample_2013$Gift.Type)
sample_2013$Gift.Type <- refactor(sample_2013$Gift.Type, c("check", "offline", "online"))

########

# donations2013$Security.Category.Name <- as.factor(donations2013$Security.Category.Name)
# donations2013$Event.ID <- as.factor(donations2013$Event.ID)
# donations2013$Public.Event.Name <- as.factor(donations2013$Public.Event.Name)
# donations2013$Campaign.Title <- as.factor(donations2013$Campaign.Title)
# donations2013$Campaign.ID <- as.factor(donations2013$Campaign.ID)
# 
# donations2013$Gift.Payment.Method <- as.factor(donations2013$Gift.Payment.Method)
# donations2013$Offline.Status <- as.factor(donations2013$Offline.Status)
# 
# summary(donations2013$Offline.Status)
# summary(donations2013$Gift.Payment.Method)

#donations2013[, c(1, 2, 3, 6, 7)] <- as.factor(donations2013[, c(1, 2, 3, 6, 7)])
# donations2013$Donor.Gender <- as.factor(donations2013$Donor.Gender)
# donations2013$Gift.Type <- as.factor(donations2013$Gift.Type)
# donations2013$Is.Prior.Participant <- as.factor(donations2013$Is.Prior.Participant)
# donations2013$Is.Team.Captain <- as.factor(donations2013$Is.Team.Captain)



cols <- c(1, 2, 3, 6, 7, 10:19, 21:32, 35, 36, 38, 39, 44:49)
donations2013[cols] <- lapply(donations2013[cols], factor)

summary(donations2013)

library(ggplot2)

ggplot(data = subset(donations2013, !is.na(Donor.Gender))) + 
  geom_bar(aes(Donor.Gender, fill = Donor.Gender), na.rm = TRUE) + 
  xlab("Donor Gender") + 
  ylab("Count") + 
  ggtitle("2013 NMS Participants by gender")

ggplot(data = subset(donations2013, !is.na(Gift.Payment.Method))) + 
  geom_bar(aes(Gift.Payment.Method, fill = Donor.Gender)) + 
  xlab("Gift Payment Method") + 
  ylab("Dollars") + 
  ggtitle("2013 NMS Gift Payment Method Types by Gender")



levels(donations2013$Donor.Gender) <- list(Female=c("Female", "F", "Feiale"), Male=c("Male", "M", "Iale"))
levels(donations2013$Gift.Type) <- list(Offline=c("offline", "ofbline"), Online="online", Check = "Check")
levels(donations2013$Is.Prior.Participant) <- list(Yes = c("TRUE", "Yes"))
levels(donations2013$Is.Team.Captain) <- list(Yes = c("TRUE"), No = c("FALSE", "0"))
levels(donations2013$Gift.Payment.Method) <- list(Cash = "Cash", Check= "Check", `Credit Card` = "Credit Card")


summary(donations2013$Donor.Gender)
summary(donations2013$Gift.Type)
summary(donations2013$Is.Prior.Participant)
summary(donations2013$Is.Team.Captain)


donations2013$Date.Recorded <- as.character(donations2013$Date.Recorded)
split_dates <- strsplit(donations2013$Date.Recorded, "/")

donations2013$Date.Recorded.Year <- lapply(split_dates, function(l) l[[3]])
donations2013$Date.Recorded.Year <- substr(donations2013$Date.Recorded.Year, start = 1, stop = 4)
donations2013$Date.Recorded.Year <- as.numeric(donations2013$Date.Recorded.Year)

donations2013$Date.Recorded.Month <- lapply(split_dates, function(l) l[[1]])
donations2013$Date.Recorded.Month <- as.numeric(donations2013$Date.Recorded.Month)

donations2013$Date.Recorded.Day <- lapply(split_dates, function(l) l[[2]])
donations2013$Date.Recorded.Day <- as.numeric(donations2013$Date.Recorded.Day)

donations2013$Date.Recorded.Time <- substr(donations2013$Date.Recorded.Year, start = 5, stop = 10)
donations2013$Date.Recorded.Time <- as.Date(donations2013$Date.Recorded.Time, format = 'h:m')

remove(split_dates)

summary(donations2013)

summary(donations2013$Donor.Gender)


donations2014 <- read.csv("C:\\Users\\Konrad\\Desktop\\ETM 527\\Donations\\2014 Bike Donations.csv")
donations2015 <- read.csv("C:\\Users\\Konrad\\Desktop\\ETM 527\\Donations\\2015 Bike Donations.csv")
donations2016 <- read.csv("C:\\Users\\Konrad\\Desktop\\ETM 527\\Donations\\2016 Bike Donations.csv")
donations2017 <- read.csv("C:\\Users\\Konrad\\Desktop\\ETM 527\\Donations\\2017 Bike Donations.csv")

donations2014$Donor.Gender <- as.factor(donations2014$Donor.Gender)
summary(donations2014$Donor.Gender)
levels(donations2014$Donor.Gender) <- list(Female=c("Female", "F", "Feiale"), Male=c("Male"))

donations2015$Donor.Gender <- as.factor(donations2015$Donor.Gender)
summary(donations2015$Donor.Gender)
levels(donations2015$Donor.Gender) <- list(Female=c("Female", "F", "Femala"), Male=c("Male"))

donations2016$Donor.Gender <- as.factor(donations2016$Donor.Gender)
summary(donations2016$Donor.Gender)
levels(donations2016$Donor.Gender) <- list(Female=c("Female", "F", "Femala"), Male=c("Male"))

donations2017$Donor.Gender <- as.factor(donations2017$Donor.Gender)
summary(donations2017$Donor.Gender)
levels(donations2017$Donor.Gender) <- list(Female=c("Female", "F", "Femala", "Famale"), Male=c("Male", "M"))

gen_2013 <- summary(donations2013$Donor.Gender)
gen_2014 <- summary(donations2014$Donor.Gender)
gen_2015 <- summary(donations2015$Donor.Gender)
gen_2016 <- summary(donations2016$Donor.Gender)
gen_2017 <- summary(donations2017$Donor.Gender)


gender_year <- data.frame(c(gen_2013[1], gen_2014[1], gen_2015[1], gen_2016[1], gen_2017[1]), 
                                 c(gen_2013[2], gen_2014[2], gen_2015[2], gen_2016[2], gen_2017[2]), 
                                 c(gen_2013[3], gen_2014[3], gen_2015[3], gen_2016[3], gen_2017[3]))
                          
colnames(gender_year) <- c("Female", "Male", "NA")
rownames(gender_year) <- 2013:2017

gender_year

ggplot(gender_year) +
  geom_histogram(aes(Male), stat = "identity") #+ 
  #geom_bar(aes(Male))

