# knnTrain Algorithm #
#--------------------#
        
#Set Directory Folder to desiried folder
setwd("c:/Users/Vasileios/Desktop/ITE/Activities")

#Source all necessary files
source('C:/Users/Vasileios/Desktop/ITE/R scripts/ReadShimmerData.R')
source('C:/Users/Vasileios/Desktop/ITE/R scripts/CreateDataFrames.R')
source('C:/Users/Vasileios/Desktop/ITE/R scripts/ExtractingFeatures.R')

#Necessary Libraries
library(class)
library(MLmetrics)
library(ROCR)
library(xlsx)

#Reading Data
data = read.csv("Dataset.CSV")

#Splitting data to training variables and the target labels
data_train = data[,-37]
data_targ = data$Target
 
#Plotting the data - 2 features at a time
plot(data[,c(1,2)], col = data_targ, pch = c(0, 1, 2, 3)[data_targ])  


###### Splitting for each activity 70% training and 30% testing (EA) #######
numberOfRepet = 100;
label = "EA"  # The splitting was done for each activity (EA)
kappa = 1;
number_of_rows_for_each_activity = 10;
size = number_of_rows_for_each_activity;
percent = 0.7;
Results <- data.frame()

for (i in 1:numberOfRepet){

indexesUp    = sample(c(1:number_of_rows_for_each_activity), (percent*size));
indexesDown  = sample(c(11:(number_of_rows_for_each_activity + 10)), (percent*size));
indexesLeft  = sample(c(21:(number_of_rows_for_each_activity + 20)), (percent*size));
indexesRight = sample(c(31:(number_of_rows_for_each_activity + 30)), (percent*size));
indexes = cbind(t(indexesUp),t(indexesDown),t(indexesLeft),t(indexesRight));

training_set = data_train[indexes,]
train_target = data_targ[indexes]
testing_set  = data_train[-indexes,]
test_target = data_targ[-indexes]

#knn(training_set, testing_set, train_target, k = kappa, prob = TRUE)
res = knn(training_set, testing_set, train_target, k = kappa)
res = as.integer(res)

# Confusion matrix
conf = confusionMatrix(res, test_target)
conf = conf[3]
conf = unlist(conf)
conf = as.double(conf)
accuracy = conf[1]

tmp = c(label, kappa, percent, accuracy, indexes)
Results <- rbind(Results, as.data.frame(t(tmp)))
}

colnames(Results) <- c("Label", "k", "Percentage %", "Accuracy", "index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index")

write.xlsx(Results, row.names = FALSE, "EA_Results.xlsx")




###### Splitting randomly 70% training and 30% testing in the whole dataset (R) #######
numberOfRepet = 100;
label = "R"  # The splitting was done randomly fro the whole dataset (R)
kappa = 1;
size = nrow(data_train);
percent = 0.7;
Results <- data.frame()

for (i in 1:numberOfRepet){

        indexes = sample(c(1:size), (percent*size));
        
        training_set = data_train[indexes,]
        train_target = data_targ[indexes]
        testing_set  = data_train[-indexes,]
        test_target = data_targ[-indexes]
        
        #knn(training_set, testing_set, train_target, k = kappa, prob = TRUE)
        res = knn(training_set, testing_set, train_target, k = kappa)
        res = as.integer(res)
        
        # Confusion matrix
        conf = confusionMatrix(res, test_target)
        conf = conf[3]
        conf = unlist(conf)
        conf = as.double(conf)
        accuracy = conf[1]
        
        tmp = c(label, kappa, percent, accuracy, indexes)
        Results <- rbind(Results, as.data.frame(t(tmp)))
}

colnames(Results) <- c("Label", "k", "Percentage %", "Accuracy", "index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index")

write.xlsx(Results, row.names = FALSE, "R_Results.xlsx")


