corr <- function(directory, threshold = 0){
  vec <- vector()
  
  for(i in 1:332){ #iterates through each file in the folder
    
    if(i<10){#creates the proper string for the filename
      value <- paste("00", i , sep = "")
    } else if((i>=10)&&(i<99)){
      value <- paste("0", i,sep ="")
    } else if(i>=100){
      value <- i
    }
    #concatenates the file name and the directory name strings and reads
    #the specific file
    fullpath         <- paste(directory,value,".csv", sep = "")
    pm_data_raw      <- read.csv(fullpath, row.names = 1)
    #cleans the data and gets rid of NA values
    pm_data_toclean  <- complete.cases(pm_data_raw)
    pm_data_clean    <- pm_data_raw[pm_data_toclean,]
    fully_observed   <- nrow(pm_data_clean)
    if (fully_observed > threshold){
      correl <- cor(pm_data_clean$sulfate, pm_data_clean$nitrate)
      vec <- c(vec, correl)
    }
  }
  vec
}