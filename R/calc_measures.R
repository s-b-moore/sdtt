#' Calculate various SDT measures.
#'
#' @description
#' Function called by user to calculate various SDT measures. Data should
#' include columns coding for proportion of hits and proportion of false
#' alarms. Data should also include outcome totals (i.e., columns titled
#' "n_hit", "n_miss", "n_cr", "n_fa") returned from get_props() function. This
#' allows automatic correction of proportions and recalculation of called
#' measure should extreme values be detected.
#'
#' @param hit_var Quoted column name coding for proportion of hits.
#'
#' @param fa_var Quoted column name coding for proportion of false alarms.
#'
#' @param measure A string indicating which measure to calculate.
#'
#' @examples
#' \dontrun{
#' # using calc_measures() with proportions data from get_props() example,
#' # passing d' as the measure to calculate
#' d_data <- calc_measures(proportions,
#'                         hit_var = "p_hit",
#'                         fa_var = "p_fa",
#'                         measure = "d_prime")
#'
#' # using calc_measures() with proportions data from get_props() example,
#' # passing criterion as the measure to calculate
#' c_data <- calc_measures(proportions,
#'                         hit_var = "p_hit",
#'                         fa_var = "p_fa",
#'                         measure = "criterion")
#'
#' # using calc_measures() with proportions data from get_props() example,
#' # passing c' as the measure to calculate
#' c_prime_data <- calc_measures(proportions,
#'                               hit_var = "p_hit",
#'                               fa_var = "p_fa",
#'                               measure = "c_prime")
#'
#' # using calc_measures() with proportions data from get_props() example,
#' # passing likelihood ratio as the measure to calculate
#' likelihood_ratio <- calc_measures(proportions,
#'                                   hit_var = "p_hit",
#'                                   fa_var = "p_fa",
#'                                   measure = "beta_lr")
#' }
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#'
#' @export

# required arguments for the function
calc_measures <- function(data,
                          hit_var = "p_hit",
                          fa_var = "p_fa",
                          measure = NULL){

  # error message if unsupported measure is called
  if(measure != "d_prime" &
     measure != "criterion" &
     measure != "c_prime" &
     measure != "beta_lr"){

    stop("Unsupported measure called. Check the 'measure' argument in the call to calc_measures().", call. = FALSE)
  }

  # error message if arguments are missing
  if(missing(hit_var) | missing(fa_var)){

    stop("Arguments for both 'hit_var' and 'fa_var' must be passed. Check each of these arguments in the call to calc_measures().", call. = FALSE)
  }

  # if measure = "d_prime"
  if(measure == "d_prime"){

    # call d_prime() function
    data <- d_prime(data,
                    hit_var = hit_var,
                    fa_var = fa_var)
  }

  # if measure = "criterion"
  if(measure == "criterion"){

    # call criterion() function
    data <- criterion(data,
                      hit_var = hit_var,
                      fa_var = fa_var)
  }

  # if measure = "c_prime"
  if(measure == "c_prime"){

    # call c_prime() function
    data <- c_prime(data,
                    hit_var = hit_var,
                    fa_var = fa_var)
  }

  # if measure = "beta_lr"
  if(measure == "beta_lr"){

    # call lr() function
    data <- lr(data,
               hit_var = hit_var,
               fa_var = fa_var)
  }

  # return data
  return(data)
}

# Calculate d'
# This wrapper function is called by the calc_measures() function when
# "d_prime" is passed as the measure to be calculated. It is not expected that
# this function be called by the user.
#
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate

# required arguments for the function
d_prime <- function(data,
                    hit_var = "p_hit",
                    fa_var = "p_fa"){

  # mutate data frame and calculate d'
  data <- data %>%
    mutate(d_prime = qnorm(data[[hit_var]]) - qnorm(data[[fa_var]]))

  # check for Inf values in d'
  data <- check(data,
                measure_var = "d_prime")

  # round d'
  data$d_prime <- round(data$d_prime, 3)

  # return data
  return(data)
}

# Calculate absolute criterion (c)
# This wrapper function is called by the calc_measures() function when
# "criterion" is passed as the measure to be calculated. It is not expected
# that this function be called by the user.
#
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate

# required arguments for the function
criterion <- function(data,
                      hit_var = "p_hit",
                      fa_var = "p_fa"){

  # mutate data frame and calculate criterion
  data <- data %>%
    mutate(criterion = -0.5 * (qnorm(data[[hit_var]]) + qnorm(data[[fa_var]])))

  # check for Inf values in criterion
  data <- check(data,
                measure_var = "criterion")

  # round criterion
  data$criterion <- round(data$criterion, 3)

  # return data
  return(data)
}

# Calculate relative criterion (c')
# This wrapper function is called by the calc_measures() function when
# "c_prime" is passed as the measure to be calculated. It is not expected
# that this function be called by the user.
#
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate

# required arguments for the function
c_prime <- function(data,
                    hit_var = "p_hit",
                    fa_var = "p_fa"){

  # mutate data frame and calculate c'
  data <- data %>%
    mutate(c_prime = -0.5 * (qnorm(data[[hit_var]]) + qnorm(data[[fa_var]])) /
             (qnorm(data[[hit_var]]) - qnorm(data[[fa_var]])))

  # check for Inf values in c'
  data <- check(data,
                measure_var = "c_prime")

  # round c'
  data$c_prime <- round(data$c_prime, 3)

  # return data
  return(data)
}

# Calculate likelihood ration (beta)
# This wrapper function is called by the calc_measures() function when
# "beta_lr" is passed as the measure to be calculated. It is not expected that
# this function be called by the user.
#
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate

# required arguments for the function
lr <- function(data,
               hit_var = "p_hit",
               fa_var = "p_fa"){

  # mutate data frame and calculate LR
  data <- data %>%
    mutate(beta_lr = log(dnorm(qnorm(data[[hit_var]])) /
                           dnorm(qnorm(data[[fa_var]]))))

  # check for Inf values in LR
  data <- check(data,
                measure_var = "beta_lr")

  # round LR
  data$beta_lr <- round(data$beta_lr, 3)

  # return data
  return(data)
}
