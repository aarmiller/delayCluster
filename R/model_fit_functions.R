
#' Extract parameters
#'
#' Extract model fit parameters for tends of visit codes prior to diagnosis
#'
#' @param data a dataset of visit counts
#' @param var_name name of variable containing visit counts (or frequencies) to
#' fit trends to
#' @param method method used to fit trends
#' @param week_effect a logical indicator of whether to include weekly periodicity
#'
#' @export
extract_parameters <- function(data, var_name = "n_visits",method, week_effect = F){
  # fit model
  fit <- delaySim::find_change_point(data,
                                     var_name=var_name,
                                     week_period = week_effect,
                                     method = method)

  # pull out parameter values
  params <- tibble(cp = -fit$change_point$period) %>%
    bind_cols(tibble(temp = names(fit$fit$coefficients),
                     vals = fit$fit$coefficients) %>%
                arrange(temp) %>%
                mutate(name = ifelse(str_detect(temp, "week"),
                                     temp, paste0("b", row_number()-1))) %>%
                select(-temp) %>%
                pivot_wider(names_from = name, values_from = vals))


  # pull out predicted values
  # preds <- fit$pred %>%
  #   as_tibble() %>%
  #   select(period,obs=Y,fitted=pred)


  return(params)
}

#' Fit trends
#'
#' Fit trends to each of the visit count codes
#'
#' @param count_data a dataset of visit counts for each code (and scaled accordingly)
#' @param param_data a dataset containing codes, names and counts to run fits over
#' @param method method used to fit trends
#' @param n_fits number of codes to fit trends to
#'
#' @export
fit_trends <- function(count_data, param_data,method = "lm",n_fits=500){

  n_iter <- min(n_fits,nrow(param_data))

  # Initializes the progress bar
  pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                       max = n_iter, # Maximum value of the progress bar
                       style = 3,
                       width = 50,
                       char = "=")   # Character used to create the bar

  param_data <- dplyr::arrange(param_data, desc(n))

  out <- list()

  for (i in 1:n_iter){

    tmp_visits_count <- count_data %>%
      dplyr::filter(code == param_data$code[i])

    out[[i]] <- extract_parameters(data = tmp_visits_count,
                                   var_name = "value",
                                   method = method,
                                   week_effect = F) %>%
      mutate(index = i)

    setTxtProgressBar(pb, i)
  }

  close(pb)

  out <- param_data %>%
    dplyr::inner_join(dplyr::bind_rows(out), by = "index") %>%
    dplyr::select(-n)

  return(out)
}
