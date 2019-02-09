complete <- function(directory, id = 1:332){
  data_list <- data.frame(id = numeric(), nobs = numeric())
  
  for(i in id){ #iterates through each file in the folder
    
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
    pm_data_raw      <- read.csv(fullpath)
    #cleans the data and gets rid of NA values
    pm_data_toclean  <- complete.cases(pm_data_raw)
    pm_data_clean    <- pm_data_raw[pm_data_toclean,]
    fully_observed   <- nrow(pm_data_clean)
    data_list_new    <- data.frame(id = i , nobs = fully_observed)
    data_list <- rbind(data_list, data_list_new)
    }
  data_list
  
}