pollutantmean <- function(directory, pollutant, id){
  total_sum <- 0
  total_length <- 0
  
  
  for(i in id){
    if(i<10){
       value <- paste("00", i , sep = "")
    } else if((i>=10)&&(i<99)){
       value <- paste("0", i,sep ="")
    } else if(i>=100){
       value <- i
    }
    fullpath <- paste(directory,value,".csv", sep = "")
    pollutant_data_raw <- read.csv(fullpath)
    pollutant_pick <- pollutant_data_raw[[pollutant]]
    pollutant_pick_to_clean <- complete.cases(pollutant_data_raw)
    pollutant_pick_cleaned <- pollutant_pick[pollutant_pick_to_clean]
    total_sum <- total_sum + sum(pollutant_pick_cleaned)
    total_length <- length(pollutant_pick_cleaned) + total_length
  }
  total_sum / total_length
}