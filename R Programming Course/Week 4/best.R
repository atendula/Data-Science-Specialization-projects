best <- function(state, outcome){
  
  outcome_pick <- NULL
  #selects the right collum index according to the chosen outcome
  
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
  
  # creates a dataframe with only the State Name, Hospital Name and 
  #chosen outcome collums
  
  raw_table <- outcome_table[c(2,7, outcome_pick)]
  
  #removes any data containing NA's , regular method was still leaving Not availables 
 
  
  
  
  #gets the list of unique states in the dataset
  states <- raw_table[["State"]]
  list_states <- unique(states) 
  
  #checks to see if the given state exists
  stateExists <- state %in% list_states # checks to see if the state is valid
  
  #checks to see if the given state and outcome are valid
  if(stateExists==FALSE){
    stop("invalid state")
    
  }else if (outcome_pick == 0){
    stop("invalid outcome")
    
  }else{
    raw_data_na_to_clean <- raw_table[, 3] == "Not Available"
    raw_table <- raw_table[!raw_data_na_to_clean, ]
    
    #forces conversion of Names and Outcome collumns
    raw_table[,3] <- as.numeric(as.character(raw_table[,3]))
    #forces conversion of collum names to characters
    raw_table[,1] <- as.character(raw_table[,1])
    
    ##gets the data from only the selected state
    raw_to_clean <- complete.cases(as.numeric(raw_table[, 3]))
    outcome_cleaned <- raw_table[raw_to_clean, ]
    
    #filters the data according to the picked state
    
    #states_to_remove <- (outcome_cleaned[["State"]] == as.character(state))
    outcome_by_state_final <- subset(outcome_cleaned, State == state)
    
    #note : the collumn for the picked outcome will always be index 3
    
    #gets the lowest value of the outcome
    
    #extracts the hospitals with the lowest outcome
    #matched_hospitals <- outcome_by_state_final[,3]== min_outcome_value[3]
    #hospitals <- outcome_by_state_final[matched_hospitals,]
    
    #sorts the data in terms of the alfabetical order names
    hospitals_sorted <- outcome_by_state_final[order(outcome_by_state_final[,3], outcome_by_state_final[,1] ), ]
    
    hospitals_sorted [1,1]
    
    #extracts the hospital name
    #as.character(hospitals_sorted[1,1])
    
  }
  
 
  
}