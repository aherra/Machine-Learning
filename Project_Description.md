## R Markdown

##Final Project Report - Practical Machine Learning Course

##Background:
 
Commercially available type devices such as Fitbit and Jawbone UP have an abled users to collect 
large volumes of exercise data devices such as these quantify the amount of movement in exercise per exercise
explosiveness or sustain of the exercises they do is not often captured this project accelerometer accelerometer
data on the arm for a belt and dumbbell of exercise participantsmachine learning stem cells have a measure of 
expected out of sample error and my choices of algorithm should be explained.

Data
https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

 
Code:

##local github repo  H:\
> ## setting directories
> setwd("//data4/Mass Spec DataRrepository/Development/Alex Data Science Cert/Machine Learning")
> 
> ##loading libraries
> 
> library(ElemStatLearn)
> library(caret)
> library(dplyr)
> library(AppliedPredictiveModeling) 
> library(ElemStatLearn)
> library(pgmm)
> library(rpart)
> library(gbm) 
> library(lubridate)
> library(forecast)
> library(e1071)
> library(rpart)
> library(rpart.plot)
> library(RColorBrewer)
> library(rattle)
> library(randomForest)
> 
> 
> set.seed(5945)
> 
> ##loading data
> training <- read.csv("pml-training.csv", na.strings=c("NA","#DIV/0!",""))
> 
> testing <- read.csv("pml-testing.csv", na.strings=c("NA","#DIV/0!",""))
> 
> ## creating data partitions
> inTrain <- createDataPartition(y=training$classe, p=0.7, list=FALSE)
> 
> myTraining <- training[inTrain, ]; myTesting <- training[-inTrain, ]
> 
> dim(myTraining); dim(myTesting)
[1] 13737   160
[1] 5885  160
> 
> ## data cleanup
> myDataNZV <- nearZeroVar(myTraining, saveMetrics=TRUE)
> 
> myTraining <- myTraining[!myNZVvars]
Error in `[.data.frame`(myTraining, !myNZVvars) : 
  object 'myNZVvars' not found
> 
> dim(myTraining)
[1] 13737   160
> 
> ##removing top
> myTraining <- myTraining[c(-1)]
> 
> ### too many NAs. If there are more than 70% remove them
> 
> trainingV3 <- myTraining #creating another subset to iterate in loop
> 
> for(i in 1:length(myTraining)) { #for every column in the training dataset
+         
+ if( sum( is.na( myTraining[, i] ) ) /nrow(myTraining) >= .7 ) { #if n?? NAs > 60% of total observations
+         
+ for(j in 1:length(trainingV3)) {
+             
+ if( length( grep(names(myTraining[i]), names(trainingV3)[j]) ) ==1)  { #if the columns are the same:
+                 trainingV3 <- trainingV3[ , -j] #Remove that column
+             }   
+         } 
+    }
+ }
> 
> dim(trainingV3)
[1] 13737    59
> 
> 
> myTraining <- trainingV3
> rm(trainingV3)
> 
> clean1 <- colnames(myTraining)
> clean2 <- colnames(myTraining[, -58])
> myTesting <- myTesting[clean1]
> testing <- testing[clean2]
Error in `[.data.frame`(testing, clean2) : undefined columns selected
> 
> 
> dim(myTesting)
[1] 5885   59
> 
> 
> dim(testing)
[1]  20 160
> 
> 
> ###making random forest and decision tree into the same type of data
> 
> for (i in 1:length(testing) ) {
+         
+ for(j in 1:length(myTraining)) {
+         
+ if( length( grep(names(myTraining[i]), names(testing)[j]) ) ==1)  {
+             class(testing[j]) <- class(myTraining[i])
+         }      
+       }      
+   }
Error in `[.data.frame`(myTraining, i) : undefined columns selected
> 
> 
> ## was the data combine successfull?
> 
> testing <- rbind(myTraining[2, -58] , testing) 
Error in rbind(deparse.level, ...) : 
  numbers of columns of arguments do not match
> 
> testing <- testing[-1,]
> 
> modFitA1 <- rpart(classe ~ ., data=myTraining, method="class")
> 
> png("decision tree.png", width=480, height=480)
> fancyRpartPlot(modFitA1)
> dev.off()
windows 
      2 
> 
> predictionsA1 <- predict(modFitA1, myTesting, type = "class")
> 
> confusionMatrix(predictionsA1, myTesting$classe)
Confusion Matrix and Statistics

          Reference
Prediction    A    B    C    D    E
         A 1598   41    4    3    0
         B   57  952   65   43    0
         C   19  137  941  154    1
         D    0    9    6  620   61
         E    0    0   10  144 1020

Overall Statistics
                                          
               Accuracy : 0.8719          
                 95% CI : (0.8631, 0.8803)
    No Information Rate : 0.2845          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.8379          
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: A Class: B Class: C Class: D Class: E
Sensitivity            0.9546   0.8358   0.9172   0.6432   0.9427
Specificity            0.9886   0.9652   0.9360   0.9846   0.9679
Pos Pred Value         0.9708   0.8523   0.7516   0.8908   0.8688
Neg Pred Value         0.9821   0.9608   0.9817   0.9337   0.9868
Prevalence             0.2845   0.1935   0.1743   0.1638   0.1839
Detection Rate         0.2715   0.1618   0.1599   0.1054   0.1733
Detection Prevalence   0.2797   0.1898   0.2127   0.1183   0.1995
Balanced Accuracy      0.9716   0.9005   0.9266   0.8139   0.9553
> 
> 
> modFitB1 <- randomForest(classe ~. , data=myTraining)
> 
> predictionsB1 <- predict(modFitB1, myTesting, type = "class")
> 
> confusionMatrix(predictionsB1, myTesting$classe)
Confusion Matrix and Statistics

          Reference
Prediction    A    B    C    D    E
         A 1674    1    0    0    0
         B    0 1138    2    0    0
         C    0    0 1024    0    0
         D    0    0    0  963    2
         E    0    0    0    1 1080

Overall Statistics
                                          
               Accuracy : 0.999           
                 95% CI : (0.9978, 0.9996)
    No Information Rate : 0.2845          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.9987          
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: A Class: B Class: C Class: D Class: E
Sensitivity            1.0000   0.9991   0.9981   0.9990   0.9982
Specificity            0.9998   0.9996   1.0000   0.9996   0.9998
Pos Pred Value         0.9994   0.9982   1.0000   0.9979   0.9991
Neg Pred Value         1.0000   0.9998   0.9996   0.9998   0.9996
Prevalence             0.2845   0.1935   0.1743   0.1638   0.1839
Detection Rate         0.2845   0.1934   0.1740   0.1636   0.1835
Detection Prevalence   0.2846   0.1937   0.1740   0.1640   0.1837
Balanced Accuracy      0.9999   0.9994   0.9990   0.9993   0.9990
> 



