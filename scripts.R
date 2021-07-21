bank <-read.csv("BankLoan Dataset 2021.csv", header = TRUE, stringsAsFactors = TRUE)
# View(bank)
# summary(bank)

bank[is.na(bank$RefNum),] # First, check for missing values

# Rows 1124 and 1125 had a majority of their features missing and it was decided that they should be removed from 
# the dataset.
bank <- bank[!is.na(bank$RefNum),]

# remove the levels that were in the row with RefNum NA's
summary(bank)
str(bank$agerange)
levels(bank$agerange)

bank$agerange <- droplevels(bank$agerange)
bank$marital <- droplevels(bank$marital)
bank$housing <- droplevels(bank$housing)
bank$loan <- droplevels(bank$loan)
bank$product <- droplevels(bank$product)
bank$month <- droplevels(bank$month)
bank$date <- droplevels(bank$date)

#add a new level to education for unknown
levels(bank$education) <- c(levels(bank$education), "unknown")
bank$education[bank$education == levels(bank$education)[1]] <- "unknown"
bank$education <- droplevels(bank$education)
summary(bank)

# add a new level to job for unknown
levels(bank$job) <- c(levels(bank$job), "unknown")
bank$job[bank$job == levels(bank$job)[1]] <- "unknown"
bank$job <- droplevels(bank$job)
summary(bank$job)

# Fixing deposit
# is the data skewed
hist(bank$deposit, breaks = 40)
lines(density(bank$deposit, na.rm = TRUE))

median_list <- aggregate(bank$deposit, by=list(bank$job), FUN=median, na.rm = TRUE)
print(median_list)

missing_deposits <- bank[is.na(bank$deposit) | bank$deposit==0,]

for (i in 1:nrow(missing_deposits)){
  missing_deposits[i,'deposit'] <- median_list[median_list$Group.1 == (missing_deposits[i,'job']),'x']
}
bank[is.na(bank$deposit) | bank$deposit==0,] <- missing_deposits

# contacted - number -1 to represents unknown
bank$contacted[is.na(bank$contacted)] <- -1

# loan value
hist(bank$loanvalue, prob = TRUE)
lines(density(bank$loanvalue, na.rm = TRUE), col = 'blue')

# replace with median
library(corrplot)
cor(bank[,c(7,12,13,14,19)], use = "complete")
corrplot(cor(bank[,c(7,12,13,14,19)], use = "complete"))
bank$loanvalue[is.na(bank$loanvalue)] <- median(bank$loanvalue, na.rm = TRUE)

#replace missing NPS
hist(bank$NPS, prob = TRUE)
lines(density(bank$NPS, na.rm = TRUE), col = 'blue')
cor(bank[,c(7,12,13,14,20)], use = "complete")
corrplot(cor(bank[,c(7,12,13,14,20)], use = "complete"))
# With NPS, 7 and 8 are passive, and are disregarded from NPS Calculations
# There replacing missing values with 7 will
bank$NPS[is.na(bank$NPS)] <- 7
summary(bank)

# CHECK NOISE
age.temp <- cut(bank$age, breaks = c(18, 30, 40, 60, 150),
                labels = c("18-30", "31-40", "41-60", "over 60"), right = TRUE)
TRUE %in% (bank$agerange == age.temp)

# balance
boxplot(bank$balance)
hist(bank$balance, prob = TRUE)
lines(density(bank$balance, col="red"))
# ignored

bank$date <- as.Date(bank$date, format= "%m/%d/%Y")
summary(bank$date)
# date is good

hist(bank$duration)
# large values

# deposit
hist(bank$deposit)
boxplot(bank$deposit)
sd_deposit <- sd(bank$deposit)
mean_deposit <- mean(bank$deposit)
upper_limit <- mean_deposit + (2*sd_deposit)
lower_limit <- mean_deposit - (2*sd_deposit)

bank$deposit[bank$deposit > upper_limit] <- upper_limit

# loan value
hist(bank$loanvalue)
boxplot(bank$loanvalue)

summary(bank)
# save file
write.csv(bank, file = "BankLoan Dataset 2021 - Clean.csv")



