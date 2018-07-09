ReadShimmerData <- function(file){
        
        #Set Directory Folder to ITE
        #setwd("c:/Users/Vasileios/Desktop/ITE")
        
        #Read RawData & colnames
        rawData = read.table(file, header=FALSE, skip=4)
        title   = read.table(file, nrows = 1, header=FALSE, skip = 2)
        title   = unlist(title)
        colnames(rawData) = title
        
        #Choose only CAL values
        indexes = ifelse(title == "CAL",1,0)
        indexes = which(indexes == 0)
        Data = rawData[,-indexes]
        
        #Omit Event_Marker and Timestamp values
        Data = Data[,-c(4,5)]
        
        #Add correct labels to values
        colnames(Data) = c("A_X", "A_Z", "A_Y", "M_Z", "M_X", "M_Y", "G_Y", "G_Z", "G_X")
        
        #Return data table
        Data
}