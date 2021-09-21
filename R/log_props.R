#' Calculate proportion of hits, misses, correct rejections, and false alarms
#' applying log-linear correction.
#'
#' @description
#' Function not expected to be called by the user. Calculates proportion of
#' each conditional probability using the log-linear correction (Hautus, 1995)
#' to account for extreme values.
#'
#' @param id_var Quoted column name coding for participant ID.
#'
#' @param conditional_var Quoted column name coding for conditional
#' probability for each row.
#'
#' @param return If set to "proportions" (default), returns a data frame
#' containing the proportions of each conditional probability. If set to
#' "raw", returns the proportions of each conditional probability as well
#' as the total number of trials for each conditional probability.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select

log_props <- function(data,
                      id_var = "id",
                      conditional_var = "con_p",
                      return = "proportions"){

  # get a unique list of participants
  ppts <- unique(data[[id_var]])

  # change conditional column to factor
  data[[conditional_var]] <- as.factor(data[[conditional_var]])

  # change participant ids variable to character
  ppts <- as.character(ppts)

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

    # correct the total number of trials
    corr_trials <- n_trials + 1

    # find the number of hits
    n_hits <- hit_data %>%
      filter(con_p == "hit") %>%
      nrow()

    # correct the number of hits
    corr_hits <- n_hits + 0.5

    # add the number of hits to raw data frame
    n_raw$n_hit[i] <- corr_hits

    # find the number of misses
    n_misses <- hit_data %>%
      filter(con_p == "miss") %>%
      nrow()

    # correct the number of misses
    corr_misses <- n_misses + 0.5

    # add the number of misses to raw data frame
    n_raw$n_miss[i] <- corr_misses

    # calculate and store the proportion of hits
    n_props$p_hit[i] <- corr_hits / corr_trials

    # calculate and store the proportion of misses
    n_props$p_miss[i] <- corr_misses / corr_trials

    # get correct rejection and false alarm data
    cr_data <- curr_ppt_data %>%
      filter(con_p == "correct_rejection" | con_p == "false_alarm")

    # find the total number of trials
    n_trials <- nrow(cr_data)

    # correct the total number of trials
    corr_trials <- n_trials + 1

    # find the number of correct rejections
    n_crs <- cr_data %>%
      filter(con_p == "correct_rejection") %>%
      nrow()

    # correct the number of correct rejections
    corr_crs <- n_crs + 0.5

    # add the number of correct rejections to raw data frame
    n_raw$n_cr[i] <- corr_crs

    # find the number of false alarms
    n_fas <- cr_data %>%
      filter(con_p == "false_alarm") %>%
      nrow()

    # correct the number of false alarms
    corr_fas <- n_fas + 0.5

    # add the number of false alarms to raw data frame
    n_raw$n_fa[i] <- corr_fas

    # calculate and store the proportion of correct rejections
    n_props$p_cr[i] <- corr_crs / corr_trials

    # calculate and store the proportion of false alarms
    n_props$p_fa[i] <- corr_fas / corr_trials
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

