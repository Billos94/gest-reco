ExtractingFeatures <- function(source = "", file_name = "", name = "", target, num =10){
        
        #Necessary Library
        library(xlsx);
        
        #Set Directory Folder to source
        path = paste("c:/Users/Vasileios/Desktop/ITE", source, sep = "");
        setwd(path);
        
        #Creating ProgressBar
        pb <- winProgressBar(title = "progress bar", min = 0, max = num, width = 300)
        setWinProgressBar(pb, 1, title=paste( round(1/num*100, 0), "% Extracting Features"))
        
        #Reading the 1st instance 
        file_name1 = paste(file_name, toString(1), ".xlsx", sep = "");
        tmp = read.xlsx(file_name1,1);
        
        #Calculate for the 1st instance the 1) Mean, 2) Median, 3) RMS, 4) STD
        Mean_Data = c(mean(tmp$A_X), mean(tmp$A_Z), mean(tmp$A_Y), mean(tmp$M_Z), mean(tmp$M_X), mean(tmp$M_Y), mean(tmp$G_Y), mean(tmp$G_Z), mean(tmp$G_X));
        Median_Data = c(median(tmp$A_X), median(tmp$A_Z), median(tmp$A_Y), median(tmp$M_Z), median(tmp$M_X), median(tmp$M_Y), median(tmp$G_Y), median(tmp$G_Z), median(tmp$G_X));
        RMS_Data = c(sqrt(mean((tmp$A_X)^2)), sqrt(mean((tmp$A_Z)^2)), sqrt(mean((tmp$A_Y)^2)), sqrt(mean((tmp$M_Z)^2)), sqrt(mean((tmp$M_X)^2)), sqrt(mean((tmp$M_Y)^2)), sqrt(mean((tmp$G_Y)^2)), sqrt(mean((tmp$G_Z)^2)), sqrt(mean((tmp$G_X)^2)));
        STD_Data = c(sd(tmp$A_X), sd(tmp$A_Z), sd(tmp$A_Y), sd(tmp$M_Z), sd(tmp$M_X), sd(tmp$M_Y), sd(tmp$G_Y), sd(tmp$G_Z), sd(tmp$G_X), target);
        data = cbind(t(Mean_Data), t(Median_Data), t(RMS_Data), t(STD_Data));
        
        for (i in 2:num){
               
                #Reading the i_th instance 
                file_name1 = paste(file_name, toString(i), ".xlsx", sep = "");
                tmp = read.xlsx(file_name1,1);
                
                #Calculate for each instance the 1) Mean, 2) Median, 3) RMS, 4) STD
                Mean_Data = c(mean(tmp$A_X), mean(tmp$A_Z), mean(tmp$A_Y), mean(tmp$M_Z), mean(tmp$M_X), mean(tmp$M_Y), mean(tmp$G_Y), mean(tmp$G_Z), mean(tmp$G_X));
                Median_Data = c(median(tmp$A_X), median(tmp$A_Z), median(tmp$A_Y), median(tmp$M_Z), median(tmp$M_X), median(tmp$M_Y), median(tmp$G_Y), median(tmp$G_Z), median(tmp$G_X));
                RMS_Data = c(sqrt(mean((tmp$A_X)^2)), sqrt(mean((tmp$A_Z)^2)), sqrt(mean((tmp$A_Y)^2)), sqrt(mean((tmp$M_Z)^2)), sqrt(mean((tmp$M_X)^2)), sqrt(mean((tmp$M_Y)^2)), sqrt(mean((tmp$G_Y)^2)), sqrt(mean((tmp$G_Z)^2)), sqrt(mean((tmp$G_X)^2)));
                STD_Data = c(sd(tmp$A_X), sd(tmp$A_Z), sd(tmp$A_Y), sd(tmp$M_Z), sd(tmp$M_X), sd(tmp$M_Y), sd(tmp$G_Y), sd(tmp$G_Z), sd(tmp$G_X), target);
                tmp1 = cbind(t(Mean_Data), t(Median_Data), t(RMS_Data), t(STD_Data));
                data = rbind(data,tmp1);
                
                #Update the progress bar
                setWinProgressBar(pb, i, title=paste( round(i/num*100, 0), "% Extracting Features"))
        }
        
        #Set the column names
        ColumnNames = c("Mean_A_X","Mean_A_Z","Mean_A_Y","Mean_M_Z","Mean_M_X","Mean_M_Y","Mean_G_Y","Mean_G_Z","Mean_G_X","Median_A_X","Median_A_Z","Median_A_Y","Median_M_Z","Median_M_X","Median_M_Y","Median_G_Y","Median_G_Z","Median_G_X","RMS_A_X","RMS_A_Z","RMS_A_Y","RMS_M_Z","RMS_M_X","RMS_M_Y","RMS_G_Y","RMS_G_Z","RMS_G_X","STD_A_X","STD_A_Z","STD_A_Y","STD_M_Z","STD_M_X","STD_M_Y","STD_G_Y","STD_G_Z","STD_G_X", "Target");
        colnames(data) <- ColumnNames;
        
        #Export the feature ectraction table
        write.xlsx(data, row.names = FALSE, name);
        
        #Close the progress bar window
        close(pb)
        
        #Return data table
        data;
}