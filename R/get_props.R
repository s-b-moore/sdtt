#' Calculate proportion of hits, misses, correct rejections, and false alarms.
#'
#' @description
#' Function called by the user to calculate proportions of each conditional
#' probability. Data should include a column containing the conditional
#' probabilities assigned using the "get_probs" function.
#'
#' @param id_var Quoted column name coding for participant ID.
#'
#' @param return If set to "proportions", returns a data frame
#' containing the proportions of each conditional probability. If set to
#' "raw" (default), returns the proportions of each conditional probability as well
#' as the total number of trials for each conditional probability.
#'
#' @param conditional_var Quoted column name coding for conditional
#' probability for each row.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select

get_props <- function(data,
                      id_var = "id",
                      probability_var = "con_p",
                      return = "raw",
                      condition_var = NULL){

  # get a unique list of participants
  ppts <- unique(data[[id_var]])

  # change conditional column to factor
  data[[probability_var]] <- as.factor(data[[probability_var]])

  # change participant ids variable to character
  ppts <- as.character(ppts)

  if(is.null(condition_var)){

    # generate a temporary data frame
    n_props <- data.frame(
      id = ppts,
      p_hit = numeric(length(ppts)),
      p_miss = numeric(length(ppts)),
      p_cr = numeric(length(ppts)),
      p_fa = numeric(length(ppts))
    )

    n_raw <- data.frame(
      id = ppts,
      n_hit = numeric(length(ppts)),
      n_miss = numeric(length(ppts)),
      n_cr = numeric(length(ppts)),
      n_fa = numeric(length(ppts))
    )

    # loop over each participant
    for(i in 1:length(ppts)){

      # get the current participant's id
      curr_ppt <- ppts[i]

      # get the current participant's data
      curr_ppt_data <- data %>%
        filter(data[[id_var]] == curr_ppt)

      # get hit and miss data
      hit_data <- curr_ppt_data %>%
        filter(con_p == "hit" | con_p == "miss")

      # find the total number of trials
      n_trials <- nrow(hit_data)

      # find the number of hits
      n_hits <- hit_data %>%
        filter(con_p == "hit") %>%
        nrow()

      # add the number of hits to raw data frame
      n_raw$n_hit[i] <- n_hits

      # find the number of misses
      n_misses <- hit_data %>%
        filter(con_p == "miss") %>%
        nrow()

      # add the number of misses to raw data frame
      n_raw$n_miss[i] <- n_misses

      # calculate and store the proportion of hits
      n_props$p_hit[i] <- n_hits / n_trials

      # calculate and store the proportion of misses
      n_props$p_miss[i] <- n_misses / n_trials

      # get correct rejection and false alarm data
      cr_data <- curr_ppt_data %>%
        filter(con_p == "correct_rejection" | con_p == "false_alarm")

      # find the total number of trials
      n_trials <- nrow(cr_data)

      # find the number of correct rejections
      n_crs <- cr_data %>%
        filter(con_p == "correct_rejection") %>%
        nrow()

      # add the number of correct rejections to raw data frame
      n_raw$n_cr[i] <- n_crs

      # find the number of false alarms
      n_fas <- cr_data %>%
        filter(con_p == "false_alarm") %>%
        nrow()

      # add the number of false alarms to raw data frame
      n_raw$n_fa[i] <- n_fas

      # calculate and store the proportion of correct rejections
      n_props$p_cr[i] <- n_crs / n_trials

      # calculate and store the proportion of false alarms
      n_props$p_fa[i] <- n_fas / n_trials
    }
  }

  if(!is.null(condition_var)){

    # get a unique list of conditions
    data$condition <- data[[condition_var]]
    conditions <- unique(data[, "condition"])

    # generate a temporary data frame
    level_props <- data.frame(
      id = ppts,
      p_hit = numeric(length(ppts)),
      p_miss = numeric(length(ppts)),
      p_cr = numeric(length(ppts)),
      p_fa = numeric(length(ppts))
    )

    level_raw <- data.frame(
      id = ppts,
      n_hit = numeric(length(ppts)),
      n_miss = numeric(length(ppts)),
      n_cr = numeric(length(ppts)),
      n_fa = numeric(length(ppts))
    )

    for(j in 1:length(conditions)){

      # get the condition data
      level_data <- data %>%
        filter(.data$condition == conditions[j])

      for(i in 1:length(ppts)){

        # get current participant's id
        curr_ppt <- ppts[i]

        # get the current participant's data
        curr_ppt_data <- level_data %>%
          filter(level_data[[id_var]] == curr_ppt)

        # get hit and miss data
        hit_data <- curr_ppt_data %>%
          filter(con_p == "hit" | con_p == "miss")

        # find the total number of trials
        n_trials <- nrow(hit_data)

        # find the number of hits
        n_hits <- hit_data %>%
          filter(con_p == "hit") %>%
          nrow()

        # add the number of hits to raw data frame
        level_raw$n_hit[i] <- n_hits

        # find the number of misses
        n_misses <- hit_data %>%
          filter(con_p == "miss") %>%
          nrow()

        # add the number of misses to raw data frame
        level_raw$n_miss[i] <- n_misses

        # calculate and store the proportion of hits
        level_props$p_hit[i] <- n_hits / n_trials

        # calculate and store the proportion of misses
        level_props$p_miss[i] <- n_misses / n_trials

        # get correct rejection and false alarm data
        cr_data <- curr_ppt_data %>%
          filter(con_p == "correct_rejection" | con_p == "false_alarm")

        # find the total number of trials
        n_trials <- nrow(cr_data)

        # find the number of correct rejections
        n_crs <- cr_data %>%
          filter(con_p == "correct_rejection") %>%
          nrow()

        # add the number of correct rejections to raw data frame
        level_raw$n_cr[i] <- n_crs

        # find the number of false alarms
        n_fas <- cr_data %>%
          filter(con_p == "false_alarm") %>%
          nrow()

        # add the number of false alarms to raw data frame
        level_raw$n_fa[i] <- n_fas

        # calculate and store the proportion of correct rejections
        level_props$p_cr[i] <- n_crs / n_trials

        # calculate and store the proportion of false alarms
        level_props$p_fa[i] <- n_fas / n_trials
      }

      # add condition data
      level_props <- level_props %>%
        mutate(condition = conditions[j])

      # bind data together
      if(j == 1){
        n_props <- level_props
        n_raw <- level_raw
      } else{
        n_props <- rbind(n_props, level_props)
        n_raw <- rbind(n_raw, level_raw)
      }
    }
  }

  # return data
  if(return == "proportions"){
    return(n_props)
  }
  if(return == "raw"){
    n_raw <- n_raw %>%
      select(-id)

    all_data <- cbind(n_props, n_raw)

    return(all_data)
  }
}

