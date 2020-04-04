## New file for R ProgrammingAssignment3
## Finding the best hospital in a state

best <- function(state, outcome) {
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
  sort_data_state[1,1]
}