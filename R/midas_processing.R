#' Get last high frequency observations
#'
#' Get last high frequency observations
#'
#' Get last high frequency observations
#' @param lf_date low frequency date
#' @param hf_data high frequency data
#' @param n_lags number of lags to add
#' @param one_row output representation
#' @return tsibble with high frequency lags added
#' @export
#' @examples
#' # no yet
get_last_n_obs = function(hf_data, lf_date, n_lags = 1, one_row = TRUE) {
  
  ts_subset = dplyr::filter(hf_data, date <= lf_date) %>% utils::tail(n_lags)
  
  n_actual = nrow(ts_subset)
  if (n_actual < n_lags) {
    # create empty data frame
    empty_df = ts_subset[FALSE, ]
    empty_df[1:(n_lags - n_actual), ] = NA
    # append it before
    ts_subset = dplyr::bind_rows(empty_df, ts_subset)
  } 
  
  if (one_row) {
    ts_subset = dplyr::mutate(ts_subset, lag_no = n_lags:1)
    ts_long = ts_subset %>% tibble::as_tibble() %>% dplyr::select(-date) %>% tidyr::gather(key = "variable", value = "value", -lag_no) %>%
      tidyr::unite(variable, lag_no, col = "var_lag", sep = "_")
    
    ts_one_row = tidyr::spread(ts_long, key = "var_lag", value = "value")
    return(ts_one_row)
  } else {
    ts_subset = dplyr::rename(ts_subset, hf_date = date)
    return(ts_subset)
  }
}


#' Transform ts to tibble
#'
#' Transform ts to tibble
#'
#' Transform ts to tibble.
#' Column with dates is called "date".
#' @param ts_data ts object
#' @return tsibble 
#' @export
#' @examples
#' test_ts = stats::ts(rnorm(100), start = c(2000, 1), freq = 12)
#' ts_2_tibble(test_ts)
ts_2_tibble = function(ts_data) {
  if (stats::is.ts(ts_data)) {
    ts_data = tsibble::as_tsibble(ts_data, gather = FALSE) %>% dplyr::rename(date = index) 
  } 
  ts_data = tibble::as_tibble(ts_data)
  return(ts_data)
}



#' Add high frequency lags to low frequency tsibble
#'
#' Add high frequency lags to low frequency tsibble
#'
#' Add high frequency lags to low frequency tsibble.
#' @param lf_data low frequency data
#' @param hf_data high frequency data
#' @param hf_variable names of high frequency time series, all by default
#' @param n_lags number of lags to add
#' @param one_row output representation
#' @return tsibble with high frequency lags added
#' @export
#' @examples
#' # no yet
add_hf_lags = function(lf_data, hf_data, hf_variable = NULL, n_lags = 1, one_row = TRUE) {
  
  # go from ts class to tsibble (dataframe)
  lf_data = ts_2_tibble(lf_data)
  hf_data = ts_2_tibble(hf_data)
  
  # if not specified take all variables
  if (is.null(hf_variable)) {
    hf_variable = setdiff(colnames(hf_data), "date")
  }
  hf_data = dplyr::select(hf_data, date, !!hf_variable) %>% stats::na.omit() 
  
  augmented_lf_data = dplyr::mutate(lf_data, hf_obs = purrr::map(date, 
                                      ~ get_last_n_obs(hf_data = hf_data, ., n_lags = n_lags, one_row = one_row)))
  augmented_lf_data = tidyr::unnest(augmented_lf_data)
  return(augmented_lf_data)
}

