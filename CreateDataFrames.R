CreateDataFrames <- function(source = "", file_name = "Down_", name ="Down", num = 10, type = "txt"){
        
        #Set Directory Folder to source
        path = paste("c:/Users/Vasileios/Desktop/ITE", source, sep = "");
        setwd(path);
        
        #Creating ProgressBar
        pb <- winProgressBar(title = "progress bar", min = 0, max = num, width = 300)
        
        #Export Data to txt or xlsx
        switch(type,
               
               txt={
                for (i in 1:num){
                file_name1 = paste(file_name, toString(i), ".dat", sep = "");
                tmp  = ReadShimmerData(file_name1);
                Data = tmp;
                name1 = paste(name, toString(i), ".txt", sep = "");
                write.table(Data, name1, row.names = FALSE, sep="\t");
                setWinProgressBar(pb, i, title=paste( round(i/num*100, 0), "% Done"))
                }
               },
               
               xlsx={
                library(xlsx);
                for (i in 1:num){
                        file_name1 = paste(file_name, toString(i), ".dat", sep = "");
                        tmp  = ReadShimmerData(file_name1);
                        Data = tmp;
                        name1 = paste(name, toString(i), ".xlsx", sep = "");
                        write.xlsx(Data, row.names = FALSE, name1);
                        setWinProgressBar(pb, i, title=paste( round(i/num*100, 0), "% Done"))
                }
                    
               },
               
               {
                print('Wrong Type:')
                print('   1) txt')
                print('   2) xlsx')
               }
        )
        close(pb)
}