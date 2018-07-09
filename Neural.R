# Neural Algorithm #
#------------------#

#Set Directory Folder to desiried folder
setwd("c:/Users/Vasileios/Desktop/ITE/Activities")

#Necessary Libraries
library(neuralnet)
library(MLmetrics)
library(ROCR)

#Reading Data
data = read.csv("Dataset.CSV")



model = neuralnet(Target ~ Mean_A_X+Mean_A_Z+Mean_A_Y+	Mean_M_Z+Mean_M_X+Mean_M_Y+Mean_G_Y+Mean_G_Z+Mean_G_X+Median_A_X+Median_A_Z+Median_A_Y+Median_M_Z+Median_M_X+Median_M_Y, data, hidden = 0, threshold = 0.000001)