bank <- read.csv("BankLoan Dataset 2021 - Clean.csv", header = TRUE, stringsAsFactors = TRUE)
View(bank)

# 1 means contacted and won, 0 means not won but contacted/unknown
bank$contacted_and_won <- bank$contacted * bank$won
bank[bank$qualified < bank$contacted,]

# -1 represents unknown if contacted
bank$qualified_and_contacted <- bank$qualified * bank$contacted

bank[bank$lead != bank$qualified,]
# 0 represents leads who are not qualified
bank$lead_and_qualified <- ifelse(bank$lead == 1 & bank$qualified == 0, 0, 1)
barplot(table(bank$lead_and_qualified))

write.csv(bank, file = "BankLoan Dataset 2021 - Clean.csv")
