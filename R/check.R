# Check for Inf values when calculating SDT measures. This wrapper function is
# called by the calc_measures() function when calculating any SDT measure. It
# is not expected that this function be called by the user.
#
# If correction is required, user will be prompted to select which correction
# to apply (only loglinear implemented in this version); proportions and the
# called measure will then be recalculated and returned to the user.
#
# For automatic correction/recalculation to run, data passed to the
# calc_measures() function must include outcome totals (i.e., columns titled
# "n_hit", "n_miss", "n_cr", "n_fa") returned from get_props() function.
#
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate


# required arguments for the function
check <- function(data,
                  measure_var = "measure"){

  # if any Inf present in the column, print message and response to console
  if(any(is.infinite(data[[measure_var]])) == TRUE |
     any(is.nan(data[[measure_var]])) == TRUE){
    response <- menu("Loglinear", title = "Extreme (Inf/NaN) value detected, correction required. Please select correction type:")

    # store the response
    if(response == 1){

      # check for outcome totals
      if("n_hit" %in% colnames(data) == FALSE |
         "n_miss" %in% colnames(data) == FALSE |
         "n_cr" %in% colnames(data) == FALSE |
         "n_fa" %in% colnames(data) == FALSE){

        stop("Automatic recalculation of proportions and measures requires totals for each outcome. Please recalculate proportions using the get_props() function and pass the entire data frame to the calc_measures() function.", call. = FALSE)
      }

      # add 0.5 to each outcome
      data <- data %>%
        mutate(n_hit = n_hit + 0.5,
               n_miss = n_miss + 0.5,
               n_cr = n_cr + 0.5,
               n_fa = n_fa + 0.5)

      # sum associated outcomes
      hit_t <- data$n_hit + data$n_miss
      cr_t <- data$n_cr + data$n_fa

      # recalculate proportions
      data <- data %>%
        mutate(p_hit = n_hit / hit_t,
               p_miss = n_miss / hit_t,
               p_cr = n_cr / cr_t,
               p_fa = n_fa / cr_t)

      # recalculate d'
      if(measure_var == "d_prime"){

        # mutate data frame and calculate d'
        data <- data %>%
          mutate(d_prime = qnorm(data$p_hit) - qnorm(data$p_fa))
      }

      # recalculate criterion
      if(measure_var == "criterion"){

        # mutate data frame and calculate criterion
        data <- data %>%
          mutate(criterion = -0.5 * (qnorm(data$p_hit) + qnorm(data$p_fa)))
      }

      # recalculate c'
      if(measure_var == "c_prime"){

        # mutate data frame and calculate c'
        data <- data %>%
          mutate(c_prime = -0.5 * (qnorm(data$p_hit) + qnorm(data$p_fa)) /
                   (qnorm(data$p_hit) - qnorm(data$p_fa)))
      }

      # recalculate LR
      if(measure_var == "beta_lr"){

        # mutate data frame and calculate LR
        data <- data %>%
          mutate(beta_lr = log(dnorm(qnorm(data$p_hit)) /
                                 dnorm(qnorm(data$p_fa))))
      }

      # return data
      return(data)
    }
  } else{
    message("No extreme (Inf/NaN) values detected. Proceed with analysis.")
    return(data)
  }
}
