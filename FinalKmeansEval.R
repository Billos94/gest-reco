#Final Kmeans Evaluation For all combinations

#Clear Workspace
#rm(list = ls())

#Set Directory Folder to desiried folder
setwd("c:/Users/Vasileios/Desktop/ITE/Results/kmeans")

#Necessary Libraries
#library(MLmetrics)
#library(ROCR)
#library("readxl")
#library(xlsx)

#Reading Data
data       = read.csv("Dataset.CSV")
centers    = read.csv("FinalAllCombinationsCenters.CSV", header = FALSE)
preds      = read.csv("FinalAllCombinationsResults.CSV", header = FALSE)
upIndex    = read.csv("up_cobminations.csv", header = FALSE)
downIndex  = read.csv("down_cobminations.csv", header = FALSE)
leftIndex  = read.csv("left_cobminations.csv", header = FALSE)
rightIndex = read.csv("right_cobminations.csv", header = FALSE)

#target is the matrix with only the information about the labels
target = data[, 37]
#data is the matrix with all the feature information except the labels
data   = data[, -37]

#Data frame for saving the results
Results_with_Index = data.frame()
Results            = data.frame()

#Number of repetitions (All the possible combinations of training set)
repet = 120

#Creating ProgressBar
pb <- winProgressBar(title = "Calculating Accuracy for all possible combinations", min = 0, max = repet, width = 300)


#Repeat for all possible combinations
for (j in 1:repet){
        
        #Updating the Progress Bar      
        setWinProgressBar(pb, j, title=paste( round(j/repet*100, 0), "% Calculating Accuracy for all possible combinations"))
        
        #testing is the 30% of the whole dataset
        tmpind        = cbind(upIndex[j,],downIndex[j,],leftIndex[j,],rightIndex[j,])
        tmpind        = as.integer(tmpind)
        testing       = data[-tmpind,]
        testingTarget = target[-tmpind]
        
        #The preds matrix contains both predicted and actual class's labels
        tmpPreds = cbind(preds[,j],target[tmpind])
        colnames(tmpPreds) <- c("kMeans_Classes", "Original_Classes")
        
        
        #Calculating the labels of the predected classes by 
        #choosing the label of the majority of activities
        #Calculating label1 for predected class 1
        Label1 = c()
        L1 = L2 = L3 = L4 = 0
        
        L1 = L1 + ifelse(tmpPreds[,2][which(tmpPreds[,1]==1)] == 1, 1,0)
        L1 = sum(L1)
        L2 = L2 + ifelse(tmpPreds[,2][which(tmpPreds[,1]==1)] == 2, 1,0)
        L2 = sum(L2)
        L3 = L3 + ifelse(tmpPreds[,2][which(tmpPreds[,1]==1)] == 3, 1,0)
        L3 = sum(L3)
        L4 = L4 + ifelse(tmpPreds[,2][which(tmpPreds[,1]==1)] == 4, 1,0)
        L4 = sum(L4)
               
        Label1 = cbind(L1,L2,L3,L4)
        colnames(Label1) = c("Actual_1", "Actual_2", "Actual_3", "Actual_4")
        rownames(Label1) = "Predected_1"
        p1 = as.integer(which.max(Label1))
        
        #Calculating label2 for predected class 2
        Label2 = c()
        L1 = L2 = L3 = L4 = 0
        
        L1 = L1 + ifelse(tmpPreds[,2][which(tmpPreds[,1]==2)] == 1, 1,0)
        L1 = sum(L1)
        L2 = L2 + ifelse(tmpPreds[,2][which(tmpPreds[,1]==2)] == 2, 1,0)
        L2 = sum(L2)
        L3 = L3 + ifelse(tmpPreds[,2][which(tmpPreds[,1]==2)] == 3, 1,0)
        L3 = sum(L3)
        L4 = L4 + ifelse(tmpPreds[,2][which(tmpPreds[,1]==2)] == 4, 1,0)
        L4 = sum(L4)
        
        Label2 = cbind(L1,L2,L3,L4)
        colnames(Label2) = c("Actual_1", "Actual_2", "Actual_3", "Actual_4")
        rownames(Label2) = "Predected_2"
        p2 = as.integer(which.max(Label2))
        
        #Calculating label3 for predected class 3
        Label3 = c()
        L1 = L2 = L3 = L4 = 0
        
        L1 = L1 + ifelse(tmpPreds[,2][which(tmpPreds[,1]==3)] == 1, 1,0)
        L1 = sum(L1)
        L2 = L2 + ifelse(tmpPreds[,2][which(tmpPreds[,1]==3)] == 2, 1,0)
        L2 = sum(L2)
        L3 = L3 + ifelse(tmpPreds[,2][which(tmpPreds[,1]==3)] == 3, 1,0)
        L3 = sum(L3)
        L4 = L4 + ifelse(tmpPreds[,2][which(tmpPreds[,1]==3)] == 4, 1,0)
        L4 = sum(L4)
        
        Label3 = cbind(L1,L2,L3,L4)
        colnames(Label3) = c("Actual_1", "Actual_2", "Actual_3", "Actual_4")
        rownames(Label3) = "Predected_3"
        p3 = as.integer(which.max(Label3))
        
        #Calculating label4 for predected class 4
        Label4 = c()
        L1 = L2 = L3 = L4 = 0
        
        L1 = L1 + ifelse(tmpPreds[,2][which(tmpPreds[,1]==4)] == 1, 1,0)
        L1 = sum(L1)
        L2 = L2 + ifelse(tmpPreds[,2][which(tmpPreds[,1]==4)] == 2, 1,0)
        L2 = sum(L2)
        L3 = L3 + ifelse(tmpPreds[,2][which(tmpPreds[,1]==4)] == 3, 1,0)
        L3 = sum(L3)
        L4 = L4 + ifelse(tmpPreds[,2][which(tmpPreds[,1]==4)] == 4, 1,0)
        L4 = sum(L4)
        
        Label4 = cbind(L1,L2,L3,L4)
        colnames(Label4) = c("Actual_1", "Actual_2", "Actual_3", "Actual_4")
        rownames(Label4) = "Predected_4"
        p4 = as.integer(which.max(Label4))
        
        
        #Replace the random selected cluster labels with the right one that we calculated earlier
        colnames(tmpPreds) = c("kMeans_Classes", "Original_Classes")
        tmp = tmpPreds
        tmpPreds[,1][tmp[,1] == 1] <- p1
        tmpPreds[,1][tmp[,1] == 2] <- p2
        tmpPreds[,1][tmp[,1] == 3] <- p3
        tmpPreds[,1][tmp[,1] == 4] <- p4
        
        #ConfusionMatrix for training clustering
        conf = caret::confusionMatrix(tmpPreds[,1], tmpPreds[,2])
        conf = conf[3]
        conf = unlist(conf)
        conf = as.double(conf)
        accuracy1 = conf[1]
       
        #Centers of predected clusters 
        pred_c1 = centers[,((4*j)-3)]
        pred_c2 = centers[,((4*j)-2)]
        pred_c3 = centers[,((4*j)-1)]
        pred_c4 = centers[,(4*j)]
        
        #Testing
        #For each testing point we calculate the nearest centroid in order to classify it in the right cluster 
        n = nrow(testing)
        res = rep(0,n)
        for (i in 1:n){
                dis1    = sum(abs(pred_c1-testing[i,]))
                dis2    = sum(abs(pred_c2-testing[i,]))
                dis3    = sum(abs(pred_c3-testing[i,]))
                dis4    = sum(abs(pred_c4-testing[i,]))
                tmp     = cbind(dis1,dis2,dis3,dis4)
                res[i]  = which.min(tmp)
        }
        
        #Replace the random selected cluster label with the right one
        tmpp = res
        res[tmpp == 1] <- p1
        res[tmpp == 2] <- p2
        res[tmpp == 3] <- p3
        res[tmpp == 4] <- p4
        
        #ConfusionMatrix for testing clustering
        conf = caret::confusionMatrix(res, testingTarget)
        conf = conf[3]
        conf = unlist(conf)
        conf = as.double(conf)
        accuracy2 = conf[1]
        
        #Creating the Results tables
        tmppp = c(accuracy2, accuracy1, tmpind)
        Results_with_Index = rbind(Results_with_Index, as.data.frame(t(tmppp)))
        
        tmpppp = c(accuracy2, accuracy1)
        Results = rbind(Results, as.data.frame(t(tmpppp)))
}

colnames(Results_with_Index) = c("Accuracy of Testing", "Accuracy of Training", "index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index")
colnames(Results) = c("Accuracy of Testing", "Accuracy of Training")

write.xlsx(Results, row.names = FALSE, "Final_All_Accuracies_Results.xlsx")

#Close the progress bar window
close(pb)
