#### Decision Tree
library(rpart)
library(rpart.plot)
library(pROC)
library(caTools)

#install.packages(party)
require(RWeka)
require(party)
require(partykit)

### Step one Load data 

data <-read.csv(file.choose())
str(data)
data$lead <- as.factor(data$lead)
str(data)

### dividing data

set.seed(33)
newDataset <-sample.split(Y=data$lead, SplitRatio = 0.7)
trainData <- data[newDataset,]
testData <- data[!newDataset,]



### Fitting Decision Tree

DTmodel <- rpart(lead ~ .,method="class", data=trainData, parms = list (split ="information gain"), control = rpart.control(minsplit = 100, maxdepth = 4))

#DTmodel <- rpart(lead ~ education + marital,method="class", data=trainData, parms = list (split ="information gain"), control = rpart.control(minsplit = 100, maxdepth = 4))

#DTmodel <- ctree(lead ~ . , data = trainData)  

DTmodel

rpart.plot(DTmodel, type=3, extra = 2, fallen.leaves = F, cex = 0.8)

