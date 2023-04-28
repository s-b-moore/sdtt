#' Plot various SDT measures.
#'
#' @description
#' Function called by user to plot various SDT measures. Data should include
#' columns containing measure values calculated by the calc_measures() function
#' as well as a column containing participant ID.
#'
#' @param id_var Quoted column name coding for participant ID.
#'
#' @param measure_var Quoted column name coding for measure values.
#'
#' @examples
#' \dontrun{
#' # using plot_measures() with d' data obtained from calc_measures() function
#' d_plot <- plot_measures(d_data,
#'                         id_var = "id",
#'                         measure = "d_prime",
#'                         condition_var = NULL)
#' }
#' @importFrom dplyr %>%
#' @importFrom forcats inorder
#' @import ggplot2
#'
#' @export

# required arguments for the function
plot_measures <- function(data,
                          id_var = "id",
                          measure = NULL,
                          condition_var = NULL){

  # error message if unsupported measure is called
  if(measure != "d_prime" &
     measure != "criterion" &
     measure != "c_prime" &
     measure != "beta_lr"){

    stop("Unsupported measure called.
         [1] Check the 'measure' argument in the call to plot_measures().",
         call. = FALSE)
  }

  # error message if arguments are missing
  if(missing(id_var)){

    stop("Argument for 'id_var' must be passed.
         [1] Check the 'id_var' argument in the call to plot_measures().",
         call. = FALSE)
  }

  # if condition_var == NULL
  if(is.null(condition_var)){

    # get total number of participants
    ppts <- length(id_var)

    # if measure == "d_prime"
    if(measure == "d_prime"){

      # check for column containing d' values
      if(!("d_prime" %in% colnames(data))){

        stop("Column for requested measure is missing from data. Please check correct 'data' and 'measure' arguments have been passed to plot_measures().")
      } else{

        # plot d' values per participant
        plot <- ggplot(data, aes(x = fct_inorder(.data[[id_var]]),
                                 y = .data[[measure]])) +
          geom_point() +
          labs(x = "Participant ID", y = "Sensitivity (d')") +
          scale_color_brewer(palette = "Dark2") +
          theme_bw()
      }
    }

    # if measure == "criterion"
    if(measure == "criterion"){

      # check for column containing absolute criterion values
      if(!("criterion" %in% colnames(data))){

        stop("Column for requested measure is missing from data. Please check correct 'data' and 'measure' arguments have been passed to plot_measures().")
      } else{

        # plot criterion values per participant
        plot <- ggplot(data, aes(x = fct_inorder(.data[[id_var]]),
                                 y = .data[[measure]])) +
          geom_point() +
          labs(x = "Participant ID", y = "Response bias (c)") +
          scale_color_brewer(palette = "Dark2") +
          theme_bw()
      }
    }

    # if measure == "c_prime"
    if(measure == "c_prime"){

      if(!("c_prime" %in% colnames(data))){

        stop("Column for requested measure is missing from data. Please check correct 'data' and 'measure' arguments have been passed to plot_measures().")
      } else{

        # plot c' values per participant
        plot <- ggplot(data, aes(x = fct_inorder(.data[[id_var]]),
                                 y = .data[[measure]])) +
          geom_point() +
          labs(x = "Participant ID", y = "Response bias (c')") +
          scale_color_brewer(palette = "Dark2") +
          theme_bw()
      }
    }

    # if measure == "beta_lr"
    if(measure == "beta_lr"){

      if(!("beta_lr" %in% colnames(data))){

        stop("Column for requested measure is missing from data. Please check correct 'data' and 'measure' arguments have been passed to plot_measures().")
      } else{

        # plot beta
        plot <- ggplot(data, aes(x = fct_inorder(.data[[id_var]]),
                                 y = .data[[measure]])) +
          geom_point() +
          labs(x = "Participant ID", y = "Response bias (\U03B2)") +
          scale_color_brewer(palette = "Dark2") +
          theme_bw()
      }
    }
  }

  # print plot
  print(plot)
}





