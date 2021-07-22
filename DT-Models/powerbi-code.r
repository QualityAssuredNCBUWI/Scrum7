# 'dataset' holds the input data for this script

#### Decision Tree
library(rpart)
library(pROC)
library(caTools)

require(party)
require(partykit)

dataset$lead <- as.factor(dataset$lead)
dataset$won <- as.factor(dataset$won)

# separate the data for an equal split
data_lead_1 = dataset[dataset$lead == 1,]
data_lead_0 = dataset[dataset$lead == 0,]

#randomize the sampling
set.seed(15)
newDataset_lead_1 <-sample.split(Y=data_lead_1$lead, SplitRatio = 0.35)
newDataset_lead_0 <-sample.split(Y=data_lead_0$lead, SplitRatio = 0.75)

trainData <-rbind(data_lead_1[newDataset_lead_1,], data_lead_0[newDataset_lead_0,])
testData <- rbind(data_lead_1[!newDataset_lead_1,][1:100,], data_lead_0[!newDataset_lead_0,])


# The . specifies all other columns ( Class ~ . )
DTmodel <- rpart(lead ~ education + age + job + marital + deposit + balance + loan + housing, method="class", data=trainData, parms = list (split ="information gain"), control = rpart.control(minsplit = 45, maxdepth = 4)) 

newdataset <- dataset
newdataset$predValue <- predict(DTmodel, dataset, type="class")
newdataset$probValue <- predict(DTmodel, dataset, type="prob")

output <- newdataset






