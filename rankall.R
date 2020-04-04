## New file for R ProgrammingAssignment3
## Ranking hospitals in all states

rankall <- function(outcome, num = "best") {
  # Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE)
  
  # Check that state and outcome are valid
  if (!outcome %in% c('heart attack', 'heart failure', 'pneumonia')) {
    stop("invalid outcome")
  }
  
  # Return hospital name in every state with lowest 30-day death rate
  # Form the name of the column we want
  if (outcome == "heart attack") {
    outcome_name <- "Heart.Attack"
  } else if (outcome == "heart failure") {
    outcome_name <- "Heart.Failure"
  } else {
    outcome_name <- "Pneumonia"
  }
  outcome_col <- paste(c("Hospital.30.Day.Death..Mortality..Rates.from.",outcome_name), collapse = "")
  
  # Pull out column for the desired outcome and remove NAs
  data_outcome <- data[,c("Hospital.Name", "State", outcome_col)]
  colnames(data_outcome) <- c("Hospital", "State", "Rating")
  data_outcome <- subset(data_outcome, Rating != "Not Available")
  data_outcome <- transform(data_outcome, Rating = as.numeric(Rating))

  # Order by death rate in ascending order separated by State, and alphabetical by tied hospitals
  sort_data_outcome <- data_outcome[order(data_outcome$State, data_outcome$Rating, data_outcome$Hospital),]
  
  # Loop through all the states and subset out requested ranked hospital and add to result data frame
  result <- data.frame()
  unique_states <- unique(sort_data_outcome$State)
  for (st in unique_states) {
    # Pull out subset of only the rows of this state
    st_sort_data_outcome <- subset(sort_data_outcome, State == st)
    
    # Adjust for if requested rank is "worst" or "best"
    if (as.character(num) == "best") {
      rank_num <- 1
    } else if (as.character(num) == "worst") {
      rank_num <- nrow(st_sort_data_outcome)
    } else {
      rank_num <- as.integer(num)
    }
    
    st_data <- st_sort_data_outcome[rank_num, c("Hospital", "State")]
    # Append state name in front too
    st_data <- cbind(st, st_data)

    # Take care of states that don't have that many hospitals
    if (rank_num > nrow(st_sort_data_outcome)) {
      st_data["State"] <- st
    }
    # Append to result
    result <- rbind(result, st_data)
  }
  # Fix column names as expected
  colnames(result) <- c("", "hospital", "state")
  result
}