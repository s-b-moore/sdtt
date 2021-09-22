#' Assign conditional probabilites
#'
#' @description
#' Function called by the user to assign conditional probabilities to data.
#' Data should include accuracy (binary) and condition (e.g., change/no change).
#'
#' @param id_var Quoted column name coding for participant ID.
#'
#' @param acc_var Quoted column name coding for accuracy.
#' Accuracy data should be binary with 1 coding for correct and 0 incorrect.
#'
#' @param signal_var Quoted column name coding for condition variable.
#' Condition data should be binary with 1 coding for signal present and
#' 0 for signal absent (e.g., change/no change; old/new).
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter

get_probs <- function(data,
                      id_var = "id",
                      accuracy_var = "accuracy",
                      condition_var = NULL,
                      signal_var = "signal_type"){

  # get a unique list of participants
  ppts <- unique(data[[id_var]])

  # create a container for the final data
  all_data <- NULL

  if(is.null(condition_var)){

    # loop over all participants
    for(curr_ppt in ppts){

      # get current participant's data
      curr_data <- data %>%
        filter(data[[id_var]] == curr_ppt)

      # assign conditional probability
      for(i in 1:nrow(curr_data)){

        if(curr_data[[signal_var]][i] == 1){
          if(curr_data$accuracy[i] == 1){
            curr_data$con_p[i] <- "hit"
          }
          if(curr_data$accuracy[i] == 0){
            curr_data$con_p[i] <- "miss"
          }
        }
        if(curr_data[[signal_var]][i] == 0){
          if(curr_data$accuracy[i] == 1){
            curr_data$con_p[i] <- "correct_rejection"
          }
          if(curr_data$accuracy[i] == 0){
            curr_data$con_p[i] <- "false_alarm"
          }
        }
      }

      # bind data
      all_data <- rbind(all_data, curr_data)
    }
  }

  if(!is.null(condition_var)){

    # get the list of conditions
    data$condition <- data[[condition_var]]
    conditions <- unique(data[, "condition"])

    # loop over each condition
    for(i in 1:length(conditions)){

      # get the current level's data
      level_data <- data %>%
        filter(data$condition == conditions[i])

      # loop over all participants
      for(curr_ppt in ppts){

        # get current participant's data
        curr_data <- level_data %>%
          filter(level_data[[id_var]] == curr_ppt)

        # assign conditional probability
        for(i in 1:nrow(curr_data)){

          if(curr_data[[signal_var]][i] == 1){
            if(curr_data$accuracy[i] == 1){
              curr_data$con_p[i] <- "hit"
            }
            if(curr_data$accuracy[i] == 0){
              curr_data$con_p[i] <- "miss"
            }
          }
          if(curr_data[[signal_var]][i] == 0){
            if(curr_data$accuracy[i] == 1){
              curr_data$con_p[i] <- "correct_rejection"
            }
            if(curr_data$accuracy[i] == 0){
              curr_data$con_p[i] <- "false_alarm"
            }
          }
        }

        # bind data
        all_data <- rbind(all_data, curr_data)
      }
    }
  }

  # return data
  return(all_data)

}

















