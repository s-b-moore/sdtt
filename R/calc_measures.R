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
#' @param measure A string indicating which measure to calculate. At present,
#' only d' and criterion measures are implemented.
#'
#' @examples
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
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate

# required arguments for the function
calc_measures <- function(data,
                          hit_var = "p_hit",
                          fa_var = "p_fa",
                          measure = NULL){

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

  # return data
  return(data)
}

# Calculate criterion
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

  # return data
  return(data)
}
