##local github repo  H:\
## setting directories
setwd("//data4/Mass Spec DataRrepository/Development/Alex Data Science Cert/Machine Learning")

##loading libraries

library(ElemStatLearn)
library(caret)
library(dplyr)
library(AppliedPredictiveModeling) 
library(caret) library(ElemStatLearn)
library(pgmm)
library(rpart)
library(gbm) 
library(lubridate)
library(forecast)
library(e1071)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)


set.seed(5945)

##loading data
training <- read.csv("pml-training.csv", na.strings=c("NA","#DIV/0!",""))

testing <- read.csv("pml-testing.csv", na.strings=c("NA","#DIV/0!",""))

## creating data partitions
inTrain <- createDataPartition(y=training$classe, p=0.7, list=FALSE)

myTraining <- training[inTrain, ]; myTesting <- training[-inTrain, ]

dim(myTraining); dim(myTesting)

## data cleanup
myDataNZV <- nearZeroVar(myTraining, saveMetrics=TRUE)

myTraining <- myTraining[!myNZVvars]

dim(myTraining)

##removing top
myTraining <- myTraining[c(-1)]

### too many NAs. If there are more than 70% remove them

trainingV3 <- myTraining #creating another subset to iterate in loop

for(i in 1:length(myTraining)) { #for every column in the training dataset
        
	if( sum( is.na( myTraining[, i] ) ) /nrow(myTraining) >= .7 ) { #if n?? NAs > 60% of total observations
        
	for(j in 1:length(trainingV3)) {
            
		if( length( grep(names(myTraining[i]), names(trainingV3)[j]) ) ==1)  { #if the columns are the same:
                trainingV3 <- trainingV3[ , -j] #Remove that column
            }   
        	} 
   		}
		}

dim(trainingV3)


myTraining <- trainingV3
rm(trainingV3)

clean1 <- colnames(myTraining)
clean2 <- colnames(myTraining[, -58])
myTesting <- myTesting[clean1]
testing <- testing[clean2]


dim(myTesting)


dim(testing)


###making random forest and decision tree into the same type of data

for (i in 1:length(testing) ) {
        
	for(j in 1:length(myTraining)) {
        
	if( length( grep(names(myTraining[i]), names(testing)[j]) ) ==1)  {
            class(testing[j]) <- class(myTraining[i])
        }      
    	  }      
	  }


## was the data combine successfull?

testing <- rbind(myTraining[2, -58] , testing) 

testing <- testing[-1,]

modFitA1 <- rpart(classe ~ ., data=myTraining, method="class")

png("decision tree.png", width=480, height=480)
fancyRpartPlot(modFitA1)
dev.off()

predictionsA1 <- predict(modFitA1, myTesting, type = "class")

confusionMatrix(predictionsA1, myTesting$classe)


modFitB1 <- randomForest(classe ~. , data=myTraining)

predictionsB1 <- predict(modFitB1, myTesting, type = "class")

confusionMatrix(predictionsB1, myTesting$classe)