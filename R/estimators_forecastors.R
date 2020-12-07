#' @title arima estimator
#' @description Envelope function that estimates arima model
#' @details Envelope function that estimates arima model. If no p, d, q are specified then auto.arima is fitted.
#' @param train_sample tsibble with train sample
#' @param predicted name of the predicted variable
#' @param predictors character with predictors separated by plus or comma, no predictors by default
#' @param options character with options separated by comma
#' Available options are: p, d, q, pseas, dseas, qseas, method.
#' @return fitted arima model
#' @export
#' @examples
#' train_sample = tsibble::tsibble(date = lubridate::ymd("2017-01-01") + months(0:99),
#'     y = arima.sim(n = 100, model = list(ar = 0.7)), x = 1:100)
#' arima_estimator(train_sample, "y", "p=2,d=0,q=0", "x")
arima_estimator = function(train_sample, predicted, options, predictors = "") {
  selected_vars = c(predicted, "date")
  y = dplyr::select(train_sample, selected_vars) %>% stats::as.ts()

  if (predictors == "") {
    regressors = NULL
  } else {
    predictors = split_variable_names(predictors)
    # important: we always use regressors as matrix (even for one predictor!)
    # this allows to store column names!
    regressors = tsibble::as_tibble(train_sample) %>% dplyr::select(predictors) %>% stats::as.ts()
  }

  options = param_string_2_tibble(options)

  if ("p" %in% colnames(options)) {
    has_order = TRUE
    pdq = c(options$p, options$d, options$q)
  } else {
    has_order = FALSE
    pdq = c(0, 0, 0)
  }

  if ("pseas" %in% colnames(options)) {
    pdq_seas = c(options$pseas, options$dseas, options$qseas)
  } else {
    pdq_seas = c(0, 0, 0)
  }

  if ("method" %in% colnames(options)) {
    method = options$method
  } else {
    method = "CSS-ML"
  }


  if (!has_order) {
    if (is.null(regressors)) {
      fit = try(forecast::auto.arima(y = y, method = method))
    } else {
      fit = try(forecast::auto.arima(y = y, xreg = regressors, method = method))
    }
  }
  if (has_order) {
    if (is.null(regressors)) {
      fit = try(forecast::Arima(y = y, order = pdq, seasonal = pdq_seas, method = method))
    } else {
      fit = try(forecast::Arima(y = y, order = pdq, seasonal = pdq_seas, method = method, xreg = regressors))
    }
  }

  return(fit)
}


#' @title arima forecastor
#' @description Envelope function that forecasts arima model
#' @details Envelope function that forecasts arima model.
#' @param fit fitted arima model
#' @param test_sample tsibble with predictors for test sample
#' @param predicted name of all the predicted variables. Not used as is determined by the fit.
#' @param predicted_ name of the perdicted variable. Not used as is determined by the fit.
#' @param predictors character with predictors separated by plus or comma, no predictors by default
#' @param options character with options separated by comma
#' not supparted
#' @param h forecasting horizon
#' @param frequency time series frequency
#' may be it is needed for forecasting with regressors, may check later
#' @return forecast object
#' @export
#' @examples
#' train_sample = tsibble::tsibble(date = lubridate::ymd("2017-01-01") + months(0:99),
#'     y = arima.sim(n = 100, model = list(ar = 0.7)), x = 1:100)
#' fit = arima_estimator(train_sample, "y", "p=2,d=0,q=0", "x")
#' test_sample = tsibble::tsibble(date = lubridate::ymd("2030-01-01") + months(0:1), x = 5:6)
#' arima_forecastor(fit, test_sample, predictors = "x", h = 2, frequency = 12)
arima_forecastor = function(fit, test_sample = NA, predicted = "", predicted_ = "", options = "", predictors = "", h = 1, frequency = 1) {
  if (predictors == "") {
    regressors = NULL
  } else {
    predictors = split_variable_names(predictors)
    # here we have the dirty trick (!)
    # if we have just one obs in test_sample then frequency in tsibble is equal to "?"
    # and automatic conversion to ts type is not possible
    # so we have automatic conversion to tibble and then to ts
    regressors = tibble::as_tibble(test_sample) %>%
      dplyr::select(predictors) %>% stats::as.ts(frequency = frequency)
  }
  if ("try-error" %in% class(fit)) {
    fcst = NA
  } else {

    if (is.null(regressors)) {
      fcst = forecast::forecast(fit, h = h)
    } else {
      fcst = forecast::forecast(fit, xreg = regressors, h = h)
    }
  }
  return(fcst)
}

#' @title tbats estimator
#' @description Envelope function that estimates tbats model
#' @details Envelope function that estimates tbats model.
#' @param train_sample tsibble with train sample
#' @param predicted name of the predicted variable
#' @param predictors not supported by tbats model
#' @param options character with options separated by comma
#' not yet supported
#' @return fitted tbats model
#' @export
#' @examples
#' train_sample = tsibble::tsibble(date = lubridate::ymd("2017-01-01") + months(0:99),
#'     y = arima.sim(n = 100, model = list(ar = 0.7)))
#' tbats_estimator(train_sample, "y", "")
tbats_estimator = function(train_sample, predicted, options, predictors = "") {
  options = param_string_2_tibble(options)
  y = dplyr::select(train_sample, !!predicted) %>% stats::as.ts()
  fit = try(forecast::tbats(y))
  return(fit)
}


#' @title tbats forecastor
#' @description Envelope function that forecasts tbats model
#' @details Envelope function that forecasts tbats model.
#' @param fit fitted tbats model
#' @param test_sample not used by tbats model
#' @param predicted name of all the predicted variables. Not used as is determined by the fit.
#' @param predicted_ name of the perdicted variable. Not used as is determined by the fit.
#' @param predictors not supported by tbats model
#' @param options character with options separated by comma
#' not supparted
#' @param h forecasting horizon
#' @param frequency time series frequency. not used by tbats model
#' @return forecast object
#' @export
#' @examples
#' train_sample = tsibble::tsibble(date = lubridate::ymd("2017-01-01") + months(0:99),
#'     y = arima.sim(n = 100, model = list(ar = 0.7)), x = 1:100)
#' fit = arima_estimator(train_sample, "y", "p=2,d=0,q=0")
#' tbats_forecastor(fit, h = 2)
tbats_forecastor = function(fit, test_sample = NA, predicted = "", predicted_ = "", options = "", predictors = "", h = 1, frequency = 1) {
  if ("try-error" %in% class(fit)) {
    fcst = NA
  } else {
    fcst = forecast::forecast(fit, h = h)
  }
  return(fcst)
}


#' @title ets estimator
#' @description Envelope function that estimates ets model
#' @details Envelope function that estimates ets model.
#' @param train_sample tsibble with train sample
#' @param predicted name of the predicted variable
#' @param predictors not supported by ets model
#' @param options character with options separated by comma
#' not yet supported
#' @return fitted ets model
#' @export
#' @examples
#' train_sample = tsibble::tsibble(date = lubridate::ymd("2017-01-01") + months(0:99),
#'     y = arima.sim(n = 100, model = list(ar = 0.7)))
#' ets_estimator(train_sample, "y", "")
ets_estimator = function(train_sample, predicted, options, predictors) {
  options = param_string_2_tibble(options)
  y = dplyr::select(train_sample, !!predicted) %>% stats::as.ts()
  fit = try(forecast::ets(y))
  return(fit)
}

#' @title ets forecastor
#' @description Envelope function that forecasts ets model
#' @details Envelope function that forecasts ets model.
#' @param fit fitted ets model
#' @param test_sample not used by ets model
#' @param predicted name of all the predicted variables. Not used as is determined by the fit.
#' @param predicted_ name of the perdicted variable. Not used as is determined by the fit.
#' @param predictors not supported by ets model
#' @param options character with options separated by comma
#' not supparted
#' @param h forecasting horizon
#' @param frequency time series frequency. not used by ets model
#' @return forecast object
#' @export
#' @examples
#' train_sample = tsibble::tsibble(date = lubridate::ymd("2017-01-01") + months(0:99),
#'     y = arima.sim(n = 100, model = list(ar = 0.7)), x = 1:100)
#' fit = arima_estimator(train_sample, "y", "p=2,d=0,q=0")
#' ets_forecastor(fit, h = 2)
ets_forecastor = function(fit, test_sample = NA, predicted = "", predicted_ = "", options = "", predictors = "", h = 1, frequency = 1) {
  if ("try-error" %in% class(fit)) {
    fcst = NA
  } else {
    fcst = forecast::forecast(fit, h = h)
  }
  return(fcst)
}

