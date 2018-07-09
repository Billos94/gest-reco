# kMeans Algorithm #
#------------------#

#Clear Workspace
#rm(list = ls())

#Set Directory Folder to desiried folder
setwd("c:/Users/Vasileios/Desktop/ITE/Results/kmeans")

#Necessary Libraries
#library(MLmetrics)
#library(ROCR)

#Reading Data
data = read.csv("Dataset.CSV")
centers = read.csv("centroids.csv", header = FALSE)
preds = read.csv("predicts.csv", header = FALSE)

#target is the matrix with only the information about the labels
target = data[, 37]
#data is the matrix all the feature information except the labels
data   = data[, -37]

#testing is the 70% of the whole dataset
testing= data[c(8:10,18:20,28:30,38:40),]
testingTarget = target[c(8:10,18:20,28:30,38:40)]

#The preds contains both predicted and actual class labels
preds = cbind(preds,target[c(1:7,11:17,21:27,31:37)])
colnames(preds) <- c("kMeans_Classes", "Original_Classes")

##SSE Calculation
#SSE <- (nrow(data) - 1) * sum(apply(data, 2, var))
#for (i in 2:10){
#        SSE[i] <- kmeans(data, centers = i)$tot.withinss
#}
#plot(1:10, SSE, type="b", xlab="Number of Clusters", ylab="SSE")

##Model
#model = kmeans(data, centers = 4)
#model$centers
#model$cluster

#model_silhouette = cluster::silhouette(model$cluster, dist(data))
#plot(model_silhouette)

#Calculation of Cityblock distance 
#orig_ci is the centers of the first point for each activity 
#pred_ci is the centers of all cluster centers
orig_c1 = data[1,]
orig_c2 = data[11,]
orig_c3 = data[21,]
orig_c4 = data[31,]

pred_c1 = centers[1,]
pred_c2 = centers[2,]
pred_c3 = centers[3,]
pred_c4 = centers[4,]

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
tmp = preds
preds$kMeans_Classes[tmp$kMeans_Classes == 1] <- p1
preds$kMeans_Classes[tmp$kMeans_Classes == 2] <- p2
preds$kMeans_Classes[tmp$kMeans_Classes == 3] <- p3
preds$kMeans_Classes[tmp$kMeans_Classes == 4] <- p4

#ConfusionMatrix
ConfusionMatrix(preds$kMeans_Classes, preds$Original_Classes)

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

caret::confusionMatrix(res, testingTarget)


