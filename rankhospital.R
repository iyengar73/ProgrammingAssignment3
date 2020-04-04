## New file for R ProgrammingAssignment3
## Ranking hospitals by outcome in a state

rankhospital <- function(state, outcome, num = "best") {
  # Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE)
  
  # Check that state and outcome are valid
  if (!(state %in% data$State)) {
    stop("invalid state")
  }
  if (!outcome %in% c('heart attack', 'heart failure', 'pneumonia')) {
    stop("invalid outcome")
  }
  
  # Return hospital name in that state with lowest 30-day death rate
  # Form the name of the column we want
  if (outcome == "heart attack") {
    outcome_name <- "Heart.Attack"
  } else if (outcome == "heart failure") {
    outcome_name <- "Heart.Failure"
  } else {
    outcome_name <- "Pneumonia"
  }
  outcome_col <- paste(c("Hospital.30.Day.Death..Mortality..Rates.from.",outcome_name), collapse = "")
  
  # Pull out only the rows and columns that we want based on the state and desired outcome
  data_state <- subset(data, State == state, select = c("Hospital.Name", outcome_col))
  colnames(data_state) <- c("Hospital", "Rating")
  data_state <- subset(data_state, Rating != "Not Available")
  data_state <- transform(data_state, Rating = as.numeric(Rating))
  
  # Order by death rate in ascending order
  sort_data_state <- data_state[order(data_state$Rating, data_state$Hospital),]
  
  # Read out the ranking asked for
  rank_num <- 1
  if (as.character(num) == "best") {
    rank_num <- 1
  } else if (as.character(num) == "worst") {
    rank_num <- nrow(sort_data_state)
  } else {
    rank_num <- as.integer(num)
  }
  if (rank_num > nrow(sort_data_state)) {
    NA
  }
  sort_data_state[rank_num,1]
}