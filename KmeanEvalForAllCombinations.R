# kMeans Evaluation for all possible combination with 70% 
#                training and 30% testing 
#---------------------------------------------------------#

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
#data is the matrix all the feature information except the labels
data   = data[, -37]

#Data frame for saving the results
Results_with_Index = data.frame()
Results            = data.frame()

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

#The preds contains both predicted and actual class labels
tmpPreds = cbind(preds[,j],target[tmpind])
colnames(tmpPreds) <- c("kMeans_Classes", "Original_Classes")

#Calculation of Cityblock distance 
#orig_ci is the centers of the first point for each activity 
#pred_ci is the centers of all cluster centers
orig_c1 = data[1,]
orig_c2 = data[11,]
orig_c3 = data[21,]
orig_c4 = data[31,]

pred_c1 = centers[,((4*j)-3)]
pred_c2 = centers[,((4*j)-2)]
pred_c3 = centers[,((4*j)-1)]
pred_c4 = centers[,(4*j)]

#disi_j  --> cityblock distance between i-th original center and j-th predicted center
dis1_1 = sum(abs(pred_c1-orig_c1))
dis1_2 = sum(abs(pred_c2-orig_c1))
dis1_3 = sum(abs(pred_c3-orig_c1))
dis1_4 = sum(abs(pred_c4-orig_c1))

dis2_1 = sum(abs(pred_c1-orig_c2))
dis2_2 = sum(abs(pred_c2-orig_c2))
dis2_3 = sum(abs(pred_c3-orig_c2))
dis2_4 = sum(abs(pred_c4-orig_c2))

dis3_1 = sum(abs(pred_c1-orig_c3))
dis3_2 = sum(abs(pred_c2-orig_c3))
dis3_3 = sum(abs(pred_c3-orig_c3))
dis3_4 = sum(abs(pred_c4-orig_c3))

dis4_1 = sum(abs(pred_c1-orig_c4))
dis4_2 = sum(abs(pred_c2-orig_c4))
dis4_3 = sum(abs(pred_c3-orig_c4))
dis4_4 = sum(abs(pred_c4-orig_c4))

distances = rbind(cbind(dis1_1, dis1_2, dis1_3, dis1_4),cbind(dis2_1, dis2_2, dis2_3, dis2_4),cbind(dis3_1, dis3_2, dis3_3, dis3_4),cbind(dis4_1, dis4_2, dis4_3, dis4_4))
colnames(distances) = c("Pred C1", "Pred C2", "Pred C3", "Pred C4")
rownames(distances) = c("Orig C1", "Orig C2", "Orig C3", "Orig C4")

#Finding the nearest reference point for each cluster in order to find which cluster label belong to which actual label
p1 = as.integer(which.min(distances[,1]))
p2 = as.integer(which.min(distances[,2]))
p3 = as.integer(which.min(distances[,3]))
p4 = as.integer(which.min(distances[,4]))

#Replace the random selected cluster label with the right one
colnames(tmpPreds) = c("kMeans_Classes", "Original_Classes")
tmp = tmpPreds
tmpPreds[,1][tmp[,1] == 1] <- p1
tmpPreds[,1][tmp[,1] == 2] <- p2
tmpPreds[,1][tmp[,1] == 3] <- p3
tmpPreds[,1][tmp[,1] == 4] <- p4

#ConfusionMatrix
conf = caret::confusionMatrix(tmpPreds[,1], tmpPreds[,2])
conf = conf[3]
conf = unlist(conf)
conf = as.double(conf)
accuracy1 = conf[1]

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

conf = caret::confusionMatrix(res, testingTarget)
conf = conf[3]
conf = unlist(conf)
conf = as.double(conf)
accuracy2 = conf[1]

tmppp = c(accuracy2, accuracy1, tmpind)
Results_with_Index = rbind(Results_with_Index, as.data.frame(t(tmppp)))


tmpppp = c(accuracy2, accuracy1)
Results = rbind(Results, as.data.frame(t(tmpppp)))
}

colnames(Results_with_Index) = c("Accuracy of Testing", "Accuracy of Training", "index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index","index")
colnames(Results) = c("Accuracy of Testing", "Accuracy of Training")

write.xlsx(Results, row.names = FALSE, "All_Accuracies_Results.xlsx")

#Close the progress bar window
close(pb)
