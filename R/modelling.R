#' Extract value column and present it as ts
#'
#' Extract value column and present it as ts
#'
#' Extract value column and present it as ts
#'
#' @param model_sample preferably tsibble 
#' @param target name of the target variable, "value" by default
#' @return univariate time series 
#' @export
#' @examples
#' test = dplyr::tibble(date = as.Date("2017-01-01") + 0:9, value = rnorm(10))
#' extract_value(test)
extract_value = function(model_sample, target = "value") {
  y = stats::as.ts(dplyr::select(model_sample, !!target))
  return(y)
}



#' Do forecast using auto ETS
#'
#' Do forecast using auto ETS
#'
#' Do forecast using auto ETS
#'
#' @param model_sample preferably tsibble with "value" column
#' @param h forecasting horizon, is ignored
#' @return auto ETS model 
#' @export
#' @examples
#' test = dplyr::tibble(date = as.Date("2017-01-01") + 0:9, value = rnorm(10))
#' ets_fun(test, 1)
ets_fun = function(model_sample, h) {
  # h is ignored!
  y = extract_value(model_sample)
  model = forecast::ets(y)
  return(model)
}


#' Do forecast using auto ARIMA
#'
#' Do forecast using auto ARIMA
#'
#' Do forecast using auto ARIMA
#'
#' @param model_sample preferably tsibble with "value" column
#' @param h forecasting horizon, is ignored
#' @return auto ARIMA model 
#' @export
#' @examples
#' test = dplyr::tibble(date = as.Date("2017-01-01") + 0:9, value = rnorm(10))
#' arima_fun(test, 1)
arima_fun = function(model_sample, h) {
  # h is ignored!
  y = extract_value(model_sample)
  model = forecast::auto.arima(y)
  return(model)
}

#' Do forecast using ARIMA(1,0,1)-SARIMA[12](1,0,1)
#'
#' Do forecast using ARIMA(1,0,1)-SARIMA[12](1,0,1)
#'
#' Do forecast using ARIMA(1,0,1)-SARIMA[12](1,0,1)
#'
#' @param model_sample preferably tsibble with "value" column
#' @param h forecasting horizon, is ignored
#' @return ARIMA(1,0,1)-SARIMA[12](1,0,1) model 
#' @export
#' @examples
#' test = dplyr::tibble(date = as.Date("2017-01-01") + 0:9, value = rnorm(10))
#' arima11_fun(test, 1)
arima11_fun = function(model_sample, h) {
  # h is ignored!
  y = extract_value(model_sample)
  model = forecast::Arima(y, order = c(1, 0, 1), seasonal = c(1, 0, 1))
  return(model)
}


#' Do forecast using auto TBATS
#'
#' Do forecast using auto TBATS
#'
#' Do forecast using auto TBATS
#'
#' @param model_sample preferably tsibble with "value" column
#' @param h forecasting horizon, is ignored
#' @return auto TBATS model 
#' @export
#' @examples
#' test = dplyr::tibble(date = as.Date("2017-01-01") + 0:9, value = rnorm(10))
#' tbats_fun(test, 1)
tbats_fun = function(model_sample, h) {
  # h is ignored!
  y = extract_value(model_sample)
  model = forecast::tbats(y)
  return(model)
}

#' Extract one scalar forecast from forecast object
#'
#' Extract one scalar forecast from forecast object
#'
#' Extract one scalar forecast from forecast object
#'
#' @param forecast_object forecast object
#' @param h forecasting horizon
#' @return mean scalar forecast
#' @export
#' @examples
#' test = dplyr::tibble(date = as.Date("2017-01-01") + 0:9, value = rnorm(10))
#' tbats = tbats_fun(test, 1)
#' fcst = forecast::forecast(tbats, h = 2)
#' forecast_2_scalar(fcst, h = 2)
forecast_2_scalar = function(forecast_object, h = 1) {
  y_hat = forecast_object$mean[h]
  return(y_hat)
}

#' Extract one scalar forecast from univariate model
#'
#' Extract one scalar forecast from univariate model
#'
#' Extract one scalar forecast from univariate model
#'
#' @param model univariate model
#' @param h forecasting horizon
#' @param model_sample ignored
#' @return mean scalar forecast
#' @export
#' @examples
#' test = dplyr::tibble(date = as.Date("2017-01-01") + 0:9, value = rnorm(10))
#' tbats = tbats_fun(test, 1)
#' uni_model_2_scalar_forecast(tbats, h = 2)
uni_model_2_scalar_forecast = function(model, h = 1, model_sample = NA) {
  # model_sample is unused in univariate models
  forecast_object = forecast::forecast(model, h = h)
  y_hat = forecast_2_scalar(forecast_object, h = h)
  return(y_hat)
}



#' Add fourier terms to tsibble
#'
#' Add fourier terms to tsibble
#'
#' Add fourier terms to tsibble
#'
#' @param original original tsibble
#' @param K_fourier number of fourier terms
#' @return tsibble with fourier terms
#' @export
#' @examples
#' test_ts = stats::ts(rnorm(100), start = c(2000, 1), freq = 12)
#' test_tsibble = tsibble::as_tsibble(test_ts)
#' add_fourier(test_tsibble)
add_fourier = function(original, K_fourier = Inf) {
  original_ts = stats::as.ts(original)
  freq = stats::frequency(original)
  K_fourier = min(floor(freq/2), K_fourier)
  
  X_fourier = forecast::fourier(original_ts, K = K_fourier)
  fourier_names = colnames(X_fourier)
  fourier_names = stringr::str_replace(fourier_names, "-", "_") %>% stringr::str_to_lower()
  colnames(X_fourier) = fourier_names
  X_fourier_tibble = tibble::as_tibble(X_fourier)
  
  augmented = dplyr::bind_cols(original, X_fourier_tibble)
  return(augmented)
}



#' Add linear and root trends to tibble
#'
#' Add linear and root trends to tibble
#'
#' Add linear and root trends to tibble
#'
#' @param original tibble
#' @return tibble with trend_lin and trend_root columns
#' @export
#' @examples
#' # dumb example: add trend to cross section :) :)
#' add_trend(cars)
add_trend = function(original) {
  nobs = nrow(original)
  augmented = dplyr::mutate(original, trend_lin = 1:nobs, trend_root = sqrt(1:nobs))
  return(augmented)
}


#' Add lags of many variables
#'
#' Add lags of many variables
#'
#' Add lags of many variables
#' The name of variables should be with quotes.
#' Designed mainly for explanatory variables.
#' @param original original tsibble
#' @param variable_names variables to add lags, with quotes! Like "gdp" and not gdp.
#' @param lags desired lags, a vector
#' @return tsibble with lags of specified variables
#' @export
#' @examples
#' test_ts = stats::ts(rnorm(100), start = c(2000, 1), freq = 12)
#' test_tsibble = tsibble::as_tsibble(test_ts)
#' add_lags(test_tsibble, "value", 1:7)
add_lags = function(original, variable_names, lags = c(1, 2)) {
  for (variable_name in variable_names) {
    for (lag in lags) {
      new_variable_name = paste0("lag", lag, "_", variable_name)
      new_value = dplyr::lag(dplyr::pull(original, variable_name), lag)
      original = dplyr::mutate(original, !!new_variable_name := new_value)
    }
  }
  return(original)
}


#' Get last date from tsibble
#'
#' Get last date from tsibble
#'
#' Get last date from tsibble
#' @param original original tsibble
#' @return last date
#' @export
#' @examples
#' test_ts = stats::ts(rnorm(100), start = c(2000, 1), freq = 12)
#' test_tsibble = tsibble::as_tsibble(test_ts)
#' get_last_date(test_tsibble)
get_last_date = function(original) {
  date_variable = tsibble::index(original)
  date = dplyr::pull(original, !!date_variable) 
  last_date = max(date)
  return(last_date)
}



#' Augment tsibble with usual regressors
#'
#' Augment tsibble with usual regressors
#'
#' Augment tsibble with usual regressors.
#' Adds trend, fourier terms, lags of regressor and dependen variables.
#' Also appends h rows for forecasting.
#' @param original original tsibble
#' @param h number of rows to append in the future
#' @param dependent name of the dependent variable
#' @return augmented tibble
#' @export
#' @examples
#' test_ts = stats::ts(rnorm(100), start = c(2000, 1), freq = 12)
#' test_tsibble = tsibble::as_tsibble(test_ts)
#' augment_tsibble_4_regression(test_tsibble, h = 4)
augment_tsibble_4_regression = function(original, dependent = "value", h = 1) {
  frequency = stats::frequency(original)
  augmented = original %>% tsibble::append_row(n = h) %>% 
    add_trend() %>% add_fourier() %>% 
    add_lags(dependent, lags = c(h, h + 1, frequency, frequency + 1))

  date_variable = tsibble::index(original)
  regressor_names = dplyr::setdiff(colnames(original), c(dependent, date_variable))
  augmented = augmented %>% add_lags(regressor_names, lags = c(h, h + 1, frequency, frequency + 1))
  augmented = dplyr::select(augmented, -!!regressor_names)
  return(augmented)
}




#' Estimate lasso model using tsibble with regressors
#'
#' Estimate lasso model using tsibble with regressors
#'
#' Estimate lasso model using tsibble with regressors
#' Regressors should already include lags, fourier terms, trend etc
#' @param augmented tsibble with all predictors with lags.
#' May be obtained using `augment_tsibble_4_regression`.
#' @param seed random seed
#' @param dependent name of the dependent variable
#' @return lasso model
#' @export
#' @examples
#' test_ts = stats::ts(rnorm(100), start = c(2000, 1), freq = 12)
#' test_tsibble = tsibble::as_tsibble(test_ts) %>% dplyr::rename(date = index)
#' augmented = augment_tsibble_4_regression(test_tsibble, h = 4)
#' model = lasso_augmented_estimate(augmented)
lasso_augmented_estimate = function(augmented, dependent = "value", seed = 777) {
  yX_tsibble = stats::na.omit(augmented)
  y = yX_tsibble %>% dplyr::pull(dependent)
  
  date_variable = tsibble::index(augmented)
  X = tibble::as_tibble(yX_tsibble) %>% dplyr::select(-!!dependent, -!!date_variable) %>% as.matrix()
  
  set.seed(seed)
  lasso_model = glmnet::cv.glmnet(X, y)
  return(lasso_model)
}


#' Estimate random forest (ranger) model using tsibble with regressors
#'
#' Estimate random forest (ranger) using tsibble with regressors
#'
#' Estimate random forest (ranger) model using tsibble with regressors.
#' Regressors should already include lags, fourier terms, trend etc
#' @param augmented tsibble with all predictors with lags.
#' May be obtained using `augment_tsibble_4_regression`.
#' @param seed random seed
#' @param dependent name of the dependent variable
#' @return lasso model
#' @export
#' @examples
#' test_ts = stats::ts(rnorm(100), start = c(2000, 1), freq = 12)
#' test_tsibble = tsibble::as_tsibble(test_ts)
#' augmented = augment_tsibble_4_regression(test_tsibble, h = 4)
#' model = ranger_augmented_estimate(augmented)
ranger_augmented_estimate = function(augmented, dependent = "value", seed = 777) {
  yX_tsibble = stats::na.omit(augmented)
  
  set.seed(seed)
  date_variable = tsibble::index(augmented)
  formula = paste0(dependent, " ~ . - ", date_variable)

  ranger_model = ranger::ranger(data = yX_tsibble, formula = formula)
  return(ranger_model)  
}



lasso_fun = function(model_sample, h = 1) {
  augmented_sample = augment_tsibble_4_regression(model_sample, h = h)
  model = lasso_augmented_estimate(augmented_sample)
  
  return(model)
}

ranger_fun = function(model_sample, h = 1) {
  augmented_sample = augment_tsibble_4_regression(model_sample, h = h)
  model = ranger_augmented_estimate(augmented_sample)
  
  return(model)
}





lasso_2_scalar_forecast = function(model, h = 1, model_sample, s = c("lambda.min", "lambda.1se")) {
  s = match.arg(s)
  
  augmented_sample = augment_tsibble_4_regression(model_sample, h = h)
  yX_future_tsibble = utils::tail(augmented_sample, 1)
  X_future = tibble::as_tibble(yX_future_tsibble) %>% dplyr::select(-value, -date) %>% as.matrix()
  
  point_forecast = stats::predict(model, X_future, s = s)
  
  return(point_forecast)
}


ranger_2_scalar_forecast = function(model, h = 1, model_sample, seed = 777) {
  
  augmented_sample = augment_tsibble_4_regression(model_sample, h = h)
  yX_future_tsibble = utils::tail(augmented_sample, 1)
  
  ranger_pred = stats::predict(model, data = yX_future_tsibble)
  point_forecast = ranger_pred$predictions
  
  return(point_forecast)
}



# for quality evaluation
prepare_model_list = function(h_all = 1, model_fun_tibble, series_data, dates_test, window_type = "sliding") {
  model_list = tidyr::crossing(date = dates_test, h = h_all, model_fun = model_fun_tibble$model_fun)
  message("You may see the warning: `.named` can no longer be a width")
  message("Don't worry :) :) Origin: crossing function")
  
  model_list = dplyr::left_join(model_list, dplyr::select(series_data, value), by = "date")
  
  model_list = dplyr::mutate(model_list, train_end_date = date - months(h * 12 / stats::frequency(series_data)))
  
  full_sample_start_date = min(series_data$date)
  full_sample_last_date = max(series_data$date)
  test_sample_start_date = min(model_list$date)
  window_min_length = round(interval(full_sample_start_date, test_sample_start_date) /  months(12 / stats::frequency(series_data))) - max(h_all) + 1
  
  
  if (window_type == "stretching") {
    model_list = dplyr::mutate(model_list, train_start_date = min(pull(series_data, date)))
  } else {
    # sliding window case
    model_list = dplyr::mutate(model_list, train_start_date = train_end_date - months((window_min_length - 1) * 12 / stats::frequency(series_data) ))
  }
  
  model_list = dplyr::mutate(model_list, 
                      train_sample = purrr::pmap(list(x = train_start_date, y = train_end_date), 
                                          ~ dplyr::filter(series_data, date >= .x, date <= .y)))
  
  
  # we estimate some models only with maximal h -----------------------------------
  
  model_list = dplyr::left_join(model_list,  model_fun_tibble, by = "model_fun")
  
  model_list = model_list %>% dplyr::group_by(train_end_date, train_start_date, model_fun) %>%
    dplyr::mutate(duplicate_model = h_agnostic & (h < max(h))) %>% dplyr::ungroup()
  
  return(model_list)
}




# for forecasts
prepare_model_list2 = function(h_all = 1, model_fun_tibble, series_data) {
  
  full_sample_last_date = as.Date(max(series_data$date))
  full_sample_start_date = as.Date(min(series_data$date))
  
  model_list = tidyr::crossing(h = h_all, model_fun = model_fun_tibble$model_fun)
  model_list = dplyr::mutate(model_list, date = full_sample_last_date + months(h * 12 / stats::frequency(series_data)))
  model_list = dplyr::mutate(model_list, train_end_date = full_sample_last_date)
  model_list = dplyr::mutate(model_list, train_start_date = full_sample_start_date)
  
  
  model_list = dplyr::mutate(model_list, 
                      train_sample = pmap(list(x = train_start_date, y = train_end_date), 
                                          ~ dplyr::filter(series_data, date >= .x, date <= .y)))
  
  
  # we estimate some models only with maximal h -----------------------------------
  
  model_list = dplyr::left_join(model_list,  model_fun_tibble, by = "model_fun")
  
  model_list = model_list %>% dplyr::group_by(train_end_date, train_start_date, model_fun) %>%
    dplyr::mutate(duplicate_model = h_agnostic & (h < max(h))) %>% dplyr::ungroup()
  return(model_list)
}


# TODO: reconsider two prepare_model functions



# models in tibble version ------------------------------------------------


estimate_nonduplicate_models = function(model_list, store_models = c("tibble", "file")) {
  store_models = match.arg(store_models)
  
  if (store_models == "file") {
    stop("File storage of models not implemented yet")
  }
  
  model_list_half_fitted = dplyr::filter(model_list, !duplicate_model)
  model_list_half_fitted = model_list_half_fitted %>% dplyr::mutate(
    fitted_model = pmap(list(train_sample, h, model_fun), ~ do.call(..3, list(h = ..2, model_sample = ..1)))
  )
  return(model_list_half_fitted)
}


# fill duplicate models ---------------------------------------------------

fill_duplicate_models = function(model_list_half_fitted, full_model_list) {
  right_tibble = model_list_half_fitted %>% dplyr::filter(h_agnostic) %>%
    dplyr::select(model_fun, train_start_date, train_end_date, fitted_model) 
  
  duplicate_models = full_model_list %>% dplyr::filter(duplicate_model)
  
  duplicate_models_fitted = dplyr::left_join(duplicate_models, right_tibble, 
                                      by = c("model_fun", "train_start_date", "train_end_date"))
  
  model_list_fitted = dplyr::bind_rows(model_list_half_fitted, duplicate_models_fitted)
  return(model_list_fitted)
}

add_point_forecasts = function(model_list_fitted) {
  model_list_fitted = dplyr::mutate(model_list_fitted, 
                             point_forecast = purrr::pmap_dbl(list(fitted_model, h, train_sample, forecast_extractor), 
                                                       ~ do.call(..4, list(model = ..1, h = ..2, model_sample = ..3))
                             ))
  return(model_list_fitted)
}

calculate_mae_table = function(model_list_fitted) {
  mae_table = model_list_fitted %>% dplyr::select(h, model_fun, value, point_forecast) %>%
    dplyr::mutate(abs_diff = abs(value - point_forecast))  %>%
    dplyr::group_by(h, model_fun) %>% dplyr::summarise(mae = mean(abs_diff))
  
  # sort by mae for each h:
  mae_table = dplyr::arrange(mae_table, h, mae) 
  
  return(mae_table)
}




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

ts_2_tibble = function(ts_data) {
  if (stats::is.ts(ts_data)) {
    ts_data = tsibble::as_tsibble(ts_data, gather = FALSE) %>% rename(date = index) 
  } 
  ts_data = tibble::as_tibble(ts_data)
  return(ts_data)
}

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









