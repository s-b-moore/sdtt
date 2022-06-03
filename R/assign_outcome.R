#' Assign outcomes (i.e., hit, miss, correct rejection, false alarm)
#'
#' @description
#' Function called by the user to assign outcomes.
#' Data should include a column coding for accuracy and a column coding for
#' signal type
#'
#' @param acc_var Quoted column name coding for accuracy.
#' Accuracy data should be binary with 1 coding for correct and 0 incorrect.
#'
#' @param signal_var Quoted column name indicating signal status
#' (e.g., old/new, change/no change). Signal data should be binary with 1
#' coding for signal present and 0 coding for signal absent
#'
#' @examples
#' \dontrun{
#' # using assign_outcome() with change detection data
#' outcome_data <- assign_outcome(my_data,
#'                                acc_var = "accuracy",
#'                                signal_var = "change_type")
#' }
#' @importFrom dplyr %>%
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#'
#' @export

# required arguments for the function
assign_outcome <- function(data,
                           acc_var = "accuracy",
                           signal_var = "signal"){

  # error message if arguments are missing
  if(missing(acc_var) | missing(signal_var)){

    stop("Arguments for both 'acc_var' and 'signal_var' must be passed. Check each of these arguments in the call to assign_outcome().", call. = FALSE)
  }

  # mutate data and assign outcomes
  data <- data %>%
    mutate(outcome = case_when(data[[acc_var]] == 1 &
                                 data[[signal_var]] == 1 ~ "hit",
                               data[[acc_var]] == 0 &
                                 data[[signal_var]] == 1 ~ "miss",
                               data[[acc_var]] == 1 &
                                 data[[signal_var]] == 0 ~ "correct_rejection",
                               data[[acc_var]] == 0 &
                                 data[[signal_var]] == 0 ~ "false_alarm"))

  # convert outcome column to a factor
  data$outcome <- as.factor(data$outcome)

  # return data
  return(data)
}
