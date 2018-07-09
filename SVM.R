# SVM Algorithm #
#------------------#

#Set Directory Folder to desiried folder
setwd("c:/Users/Vasileios/Desktop/ITE/Results/SVM")

#Source all necessary files
source('C:/Users/Vasileios/Desktop/ITE/R scripts/ReadShimmerData.R')
source('C:/Users/Vasileios/Desktop/ITE/R scripts/CreateDataFrames.R')
source('C:/Users/Vasileios/Desktop/ITE/R scripts/ExtractingFeatures.R')

#Necessary Libraries
library(e1071)
library(MLmetrics)
library(ROCR)
library(xlsx)

#Reading Data
data = read.csv("Dataset.CSV")

###### Splitting for each activity 70% training and 30% testing (EA) #######
numberOfRepet = 100;
#label = "linear"
label = "sigmoid"
number_of_rows_for_each_activity = 10;
size = number_of_rows_for_each_activity;
percent = 0.7;
Results <- data.frame()

for (i in 1:numberOfRepet){
        
indexesUp    = sample(c(1:number_of_rows_for_each_activity), (percent*size));
indexesDown  = sample(c(11:(number_of_rows_for_each_activity + 10)), (percent*size));
indexesLeft  = sample(c(21:(number_of_rows_for_each_activity + 20)), (percent*size));
indexesRight = sample(c(31:(number_of_rows_for_each_activity + 30)), (percent*size));
indexes      = cbind(t(indexesUp),t(indexesDown),t(indexesLeft),t(indexesRight));
        
training_set = data[indexes,]
testing_set  = data[-indexes,]
       
model=svm(training_set$Target~.,data=training_set,kernel="linear") 
res = predict(model, testing_set[,-37])
res = round(res)
res = ifelse(res>4,4,res)
res = ifelse(res<1,1,res)
res = as.integer(res)


# Confusion matrix
conf = confusionMatrix(res, testing_set$Target)
conf = conf[3]
conf = unlist(conf)
conf = as.double(conf)
accuracy = conf[1]

#tmp = c(label, percent, accuracy, indexes)
tmp = c(label, percent, accuracy)
Results <- rbind(Results, as.data.frame(t(tmp)))
}

#colnames(Results) <- c("Label", "Percentage %", "Accuracy", "index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index")
colnames(Results) <- c("Label", "Percentage %", "Accuracy")

write.xlsx(Results, row.names = FALSE, "SVM_Results_sigmoid_0.7.xlsx")

