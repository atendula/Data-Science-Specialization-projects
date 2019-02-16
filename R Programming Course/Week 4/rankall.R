rankall <- function(outcome, num = "best"){
  
  outcome_pick <- NULL
  #selects the right collum index according to the chosen outcome
  hospital_name <- NULL
  
  if (outcome == "heart attack"){
    outcome_pick <- 11
  }else if (outcome == "pneumonia"){
    outcome_pick <- 23
  }else if(outcome == "heart failure"){
    outcome_pick <- 17
  } else {
    outcome_pick <- 0
  }
  
  outcome_table <- read.csv("outcome-of-care-measures.csv")
  
  outcome_pick
  # creates a dataframe with only the State Name, Hospital Name and 
  #chosen outcome collums
  
  raw_table <- outcome_table[c(2,7, as.numeric(outcome_pick))]
  
  raw_data_na_to_clean <- raw_table[, 3] == "Not Available"
  raw_table <- raw_table[!raw_data_na_to_clean, ]
  
  raw_table[,3] <- as.numeric(as.character(raw_table[,3]))
  #forces conversion of collum names to characters
  raw_table[,1] <- as.character(raw_table[,1])
  
  
  #creates the dataframe for holding the result data
  
  out_df <- data.frame(hospital = character(), state = character())
  
  #gets the list of unique states in the dataset
  states <- raw_table[["State"]]
  list_states <- unique(states)

  ##Cleans the data and removes any missing rows with the outcome information missing
  raw_to_clean <- complete.cases(as.numeric(raw_table[, 3]))
  outcome_data_cleaned <- raw_table[raw_to_clean, ]
  
  #removes any data containing NA's , regular method was still leaving Not availables 
  
  #note : the collumn for the picked outcome will always be index 3
  
  for (state in list_states){
    #filters the table for each state
    
    outcome_by_state_final <- subset(outcome_data_cleaned, State == state) 
    
    if (num == "best"){
      
      #sorts the data in terms of the outcome in ascending and selects the very first item in the table
      hospitals_sorted <- outcome_by_state_final[order(outcome_by_state_final[,3],outcome_by_state_final[,1]), ]
      hospital_name <- as.character(hospitals_sorted[1,1])
      
    }else if (num == "worst"){
      #sorts the data in a descending way and selects the very first item in the table
      hospitals_sorted <- outcome_by_state_final[order(-outcome_by_state_final[,3],outcome_by_state_final[,1]), ]
      hospital_name <- as.character(hospitals_sorted[1,1])
      ### check error here
      
    }else if(is.numeric(num)& (num>0)){
      #sorts the data in an ascending manner in terms of the outcome and the hospital name, and selects the NUM item in the table
      hospitals_sorted <- outcome_by_state_final[order(as.numeric(outcome_by_state_final[,3]), outcome_by_state_final[,1]), ]
      
      #ensures that a result is only returned when the num argument is less or equal than the number of rows
      if(nrow(hospitals_sorted)>=num){
        hospital_name <- as.character(hospitals_sorted[num,1])
        #hospitals_sorted
      }else{
        hospital_name <- NA
      }
      
    }
    
    into_df <- data.frame(hospital = hospital_name, state = state)
    out_df <- rbind(out_df, into_df)
    
  }
  
  out_df
  
}