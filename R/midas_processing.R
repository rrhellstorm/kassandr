#' Get last high frequency observations
#'
#' Get last high frequency observations
#'
#' Get last high frequency observations. The function cuts n_lags
#' observations from the table hf_data, that are from the same day or before
#' as the date indicated by lf_date.
#' If one_row is TRUE the result is stacked in a row, otherwise
#' the result is a column.
#' @param lf_date low frequency date
#' @param hf_data high frequency data
#' @param n_lags number of lags to add
#' @param one_row output representation
#' @return tsibble with high frequency lags added
#' @export
#' @examples
#' hf_data <- tibble::tibble(date = lubridate::ymd("2011-01-01") +
#'            lubridate::days(0:60), observations = rnorm(61))
#' get_last_n_obs(hf_data, "2011-02-23", n_lags = 7,
#'            one_row = FALSE)
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
#' Transform ts to tibble. The function takes univariate or multivariate
#' time series and transforms them to a tibble with a separate column indicating
#' a period of time.
#' Column with dates is called "date".
#' @param ts_data ts object
#' @return tsibble
#' @export
#' @examples
#' test_ts = stats::ts(rnorm(100), start = c(2000, 1), freq = 12)
#' ts_2_tibble(test_ts)
ts_2_tibble = function(ts_data) {
  if (stats::is.mts(ts_data)) {
    ts_data = tsibble::as_tsibble(ts_data, pivot_longer = FALSE) %>% dplyr::rename(date = index)
  } else if (stats::is.ts(ts_data)) {
    ts_data = tsibble::as_tsibble(ts_data) %>% dplyr::rename(date = index)
  }
  ts_data = tibble::as_tibble(ts_data)
  return(ts_data)
}


#' Add high frequency lags to low frequency tsibble
#'
#' Add high frequency lags to low frequency tsibble
#'
#' Adds high frequency lags to low frequency tsibble. The function
#' concatenates n_lags of hf_data to lf_data. The concatenated observations
#' from hf_data are from the same day or earlier than the date indicated
#' by lf_date. If one_row is TRUE the high frequency observations
#' are put in a row so as the number of rows in the new table is equal
#' to those of lf_data.  Otherwise the lags of high-frequency data
#' increase the number of rows and the number of rows in
#' new table is (number of rows in lf_data)*n_lags.
#' @param lf_data low frequency data
#' @param hf_data high frequency data
#' @param hf_variable names of high frequency time series, all by default
#' @param n_lags number of lags to add
#' @param one_row output representation
#' @return tsibble with high frequency lags added
#' @export
#' @examples
#'
#' hf_data <- tibble::tibble(date = lubridate::ymd("2011-01-01") +
#'                             lubridate::days(0:90), observations = rnorm(91))
#' lf_data <- stats::ts(rnorm(4), start = c(2011, 01), frequency = 12)
#' add_hf_lags(lf_data, hf_data, n_lags = 5)
add_hf_lags = function(lf_data, hf_data, hf_variable = NULL, n_lags = 1, one_row = TRUE) {

  #lf_data_ts <- ts_2_tibble(lf_data)
  #lf_data_ts <- mutate(lf_data_ts, date = as.Date(date) + months(1) - days(1))

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
  augmented_lf_data = tidyr::unnest(augmented_lf_data, cols = c(hf_obs))
  return(augmented_lf_data)
}

