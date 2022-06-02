#' Calculate proportion of hits, misses, correct rejections, and false alarms
#' and return totals for each outcome.
#'
#' @description
#' Function called by the user to calculate proportions of each outcome.
#' Data should include a column containing the outcomes assigned using the
#' assign_outcome() function as well as a column containing participant ID.
#'
#' @param id_var Quoted column name coding for participant ID.
#'
#' @param outcome_var Quoted column name coding for outcomes assigned by the
#' assign_outcome() function.
#'
#' @param condition_var Quoted column name coding for condition.
#'
#' @param correction A string indicating which correction to apply when
#' calculating proportions. At present, only the loglinear correction
#' (see Hautus, 1995) is implemented. A check will be run during calculation of
#' SDT measures which recalculates proportions with correction if necessary.
#' Therefore, in most cases it would be unnecessary to specify correction
#' here.
#'
#' @examples
#' # using get_props() with outcome data from assign_outcome() example with no
#' # condition or correction arguments passed
#' proportions <- get_props(outcome_data,
#'                          id_var = "id",
#'                          outcome_var = "outcome",
#'                          condition_var = NULL,
#'                          correction = NULL)
#'
#' # using get_props() with outcome data from assign_outcome() example with
#' condition argument passed
#' proportions <- get_props(outcome_data,
#'                          id_var = "id",
#'                          outcome_var = "outcome",
#'                          condition_var = "sequence",
#'                          correction = NULL)
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter

# required arguments for the function
get_props <- function(data,
                      id_var = "id",
                      outcome_var = "outcome",
                      condition_var = NULL,
                      correction = NULL){

  # get a unique vector of participants
  ppts <- unique(data[[id_var]])

  # convert participant id variable to character
  ppts <- as.character(ppts)

  # if condition_var is null
  if(is.null(condition_var)){

    # generate a data frame to store totals
    props <- data.frame(
      id = ppts,
      n_hit = numeric(length(ppts)),
      p_hit = numeric(length(ppts)),
      n_miss = numeric(length(ppts)),
      p_miss = numeric(length(ppts)),
      n_cr = numeric(length(ppts)),
      p_cr = numeric(length(ppts)),
      n_fa = numeric(length(ppts)),
      p_fa = numeric(length(ppts))
    )

    # loop over each participant
    for(i in 1:length(ppts)){

      # get the current participants id
      curr_ppt <- ppts[i]

      # get the current participants data
      curr_ppt_data <- data %>%
        filter(data[[id_var]] == curr_ppt)

      # add totals to props data frame
      props$n_hit[i] <- curr_ppt_data %>%
        filter(outcome == "hit") %>%
        nrow()

      props$n_miss[i] <- curr_ppt_data %>%
        filter(outcome == "miss") %>%
        nrow()

      props$n_cr[i] <- curr_ppt_data %>%
        filter(outcome == "correct_rejection") %>%
        nrow()

      props$n_fa[i] <- curr_ppt_data %>%
        filter(outcome == "false_alarm") %>%
        nrow()

      # if correction is null
      if(is.null(correction)){

        # calculate proportion for each outcome
        props$p_hit[i] <- props$n_hit[i] /
          sum(props$n_hit[i], props$n_miss[i])

        props$p_miss[i] <- props$n_miss[i] /
          sum(props$n_hit[i], props$n_miss[i])

        props$p_cr[i] <- props$n_cr[i] /
          sum(props$n_cr[i], props$n_fa[i])

        props$p_fa[i] <- props$n_fa[i] /
          sum(props$n_cr[i], props$n_fa[i])

      } else if(correction == "loglinear"){

        # calculate proportion for each outcome using loglinear correction
        props$p_hit[i] <- (props$n_hit[i] + 0.5) /
          (sum(props$n_hit[i], props$n_miss[i]) + 1)

        props$p_miss[i] <- (props$n_miss[i] + 0.5) /
          (sum(props$n_hit[i], props$n_miss[i]) + 1)

        props$p_cr[i] <- (props$n_cr[i] + 0.5) /
          (sum(props$n_cr[i], props$n_fa[i]) + 1)

        props$p_fa[i] <- (props$n_fa[i] + 0.5) /
          (sum(props$n_cr[i], props$n_fa[i]) + 1)
      }
    }
  }

  # if condition_var is not null
  if(!is.null(condition_var)){

    props <- gp(data,
                id_var = id_var,
                outcome_var = outcome_var,
                condition_var = condition_var,
                correction = correction)
  }

  # return data
  return(props)
}

# Calculate proportion of hits, misses, correct rejections and false alarms
# and return totals for different conditions.
#
# This wrapper function is called by the get_props() function when
# condition_var is not null. It is not expected that this function be called by
# the user.
#
#' @importFrom dplyr %>%
#' @importFrom dplyr filter

# required arguments for the function
gp <- function(data,
               id_var = "id",
               outcome_var = "outcome",
               condition_var = NULL,
               correction = NULL){

  # get a unique vector of participants
  ppts <- unique(data[[id_var]])

  # convert participant id variable to character
  ppts <- as.character(ppts)

  # if condition_var is null
  if(!is.null(condition_var)){

    # get a unique vector of conditions
    data$condition <- data[[condition_var]]
    conditions <- unique(data[, "condition"])

    # generate a data frame to store totals
    cond_props <- data.frame(
      id = ppts,
      n_hit = numeric(length(ppts)),
      p_hit = numeric(length(ppts)),
      n_miss = numeric(length(ppts)),
      p_miss = numeric(length(ppts)),
      n_cr = numeric(length(ppts)),
      p_cr = numeric(length(ppts)),
      n_fa = numeric(length(ppts)),
      p_fa = numeric(length(ppts))
    )

    # loop over each condition
    for(j in 1:length(conditions)){

      # get the current condition data
      curr_cond_data <- data %>%
        filter(.data$condition == conditions[j])

      # add condition information to props
      cond_props <- cond_props %>%
        mutate(condition = conditions[j])

      # loop over each participant
      for(i in 1:length(ppts)){

        # get the current participants id
        curr_ppt <- ppts[i]

        # get the current participants data
        curr_ppt_data <- curr_cond_data %>%
          filter(curr_cond_data[[id_var]] == curr_ppt)

        # add totals to props data frame
        cond_props$n_hit[i] <- curr_ppt_data %>%
          filter(outcome == "hit") %>%
          nrow()

        cond_props$n_miss[i] <- curr_ppt_data %>%
          filter(outcome == "miss") %>%
          nrow()

        cond_props$n_cr[i] <- curr_ppt_data %>%
          filter(outcome == "correct_rejection") %>%
          nrow()

        cond_props$n_fa[i] <- curr_ppt_data %>%
          filter(outcome == "false_alarm") %>%
          nrow()

        if(is.null(correction)){

          # calculate proportion for each outcome
          cond_props$p_hit[i] <- cond_props$n_hit[i] /
            sum(cond_props$n_hit[i], cond_props$n_miss[i])

          cond_props$p_miss[i] <- cond_props$n_miss[i] /
            sum(cond_props$n_hit[i], cond_props$n_miss[i])

          cond_props$p_cr[i] <- cond_props$n_cr[i] /
            sum(cond_props$n_cr[i], cond_props$n_fa[i])

          cond_props$p_fa[i] <- cond_props$n_fa[i] /
            sum(cond_props$n_cr[i], cond_props$n_fa[i])

        } else if(correction == "loglinear"){

          # calculate proportion for each outcome using loglinear correction
          cond_props$p_hit[i] <- (cond_props$n_hit[i] + 0.5) /
            (sum(cond_props$n_hit[i], cond_props$n_miss[i]) + 1)

          cond_props$p_miss[i] <- (cond_props$n_miss[i] + 0.5) /
            (sum(cond_props$n_hit[i], cond_props$n_miss[i]) + 1)

          cond_props$p_cr[i] <- (cond_props$n_cr[i] + 0.5) /
            (sum(cond_props$n_cr[i], cond_props$n_fa[i]) + 1)

          cond_props$p_fa[i] <- (cond_props$n_fa[i] + 0.5) /
            (sum(cond_props$n_cr[i], cond_props$n_fa[i]) + 1)

        }
      }

      if(j == 1){
        props <- cond_props
      } else{
        props <- rbind(props, cond_props)
      }
    }
  }

  # return data
  return(props)
}
