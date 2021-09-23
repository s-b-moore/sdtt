#' Calculate d'.
#'
#' @description
#' Function called by user to calculate d'. Data should include columns coding
#' for proportion of hits and proportion of false alarms.
#'
#' @param id_var Quoted column name coding for participant ID.
#'
#' @param hit_var Quoted column name coding for proportion of hits.
#'
#' @param fa_var Quoted column name coding for proportion of false alarms.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate

d_prime <- function(data,
                    id_var = "id",
                    hit_var = "p_hit",
                    fa_var = "p_fa"){

  # mutate data frame and calculate d'
  data <- data %>%
    mutate(d_prime = qnorm(data[[hit_var]]) - qnorm(data[[fa_var]]))

  # loop over data
  for(i in 1:nrow(data)){
    if(is.infinite(data$d_prime[i])){
      data$check[i] <- 1
    } else{
      data$check[i] <- 0
    }
  }

  if(sum(data$check >= 1)){

    response <- menu("Log-linear", title = "Extreme value detected, correction required. Please select correction type:")

    if(response == 1){

      data <- data %>%
        mutate(n_hit = n_hit + 0.5) %>%
        mutate(n_miss = n_miss + 0.5) %>%
        mutate(n_cr = n_cr + 0.5) %>%
        mutate(n_fa = n_fa + 0.5)

      hit_t <- data$n_hit + data$n_miss
      cr_t <- data$n_cr + data$n_fa

      data <- data %>%
        mutate(p_hit = n_hit / hit_t) %>%
        mutate(p_miss = n_miss / hit_t) %>%
        mutate(p_cr = n_cr / cr_t) %>%
        mutate(p_fa = n_fa / cr_t)

      data <- data %>%
        mutate(d_prime = qnorm(p_hit) - qnorm(p_fa))

      data <- data %>%
        select(-check)
    }
  } else{
    print("Proceed with analysis.")
  }

  # return data
  return(data)
}

