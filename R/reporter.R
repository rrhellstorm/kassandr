#' @title Produces report table
#' @description Produces table with additional information concerning downloaded data.
#' @details Several metrics to detect potentional download errors.
#' @param download_log metadata about downloaded series
#' @param path path to specific day
#' @return data.frame
#' @export
#' @examples
#' \donttest{
#' path <- "C:/CMF/Github repos/data/raw/"
#' path_day <- paste0(path, "/", Sys.Date(), "/") # add current date to path
#' download_log_new <- download_statistics(path, watchdog)
#' report_table <- make_report(download_log_new, path_day)
#' }
make_report <- function(download_log, path, SUCCESS = "success") {
  result_table <- data.frame(
    "ts_name" = character(), "comment" = character(), "freq" = integer(),
    "file_name" = character(), "numeric_values" = logical(),
    "obs_num" = integer(),
    "first_obs_date" = as.Date(character()),
    "last_obs_date" = as.Date(character()), "NA_cnt" = integer(),
    "omitted_dates_cnt" = integer(), "corr_t_t.1" = numeric(),
    "corr_t_t.freq" = numeric(), "MAPE_naive" = numeric(),
    "MAPE_Snaive" = numeric(), "MAPE_auto_ets" = numeric()
  )

  for (file_num in 1:nrow(download_log)) {
    file <- download_log[file_num, ]
    if (file$processing_status != SUCCESS) {
      warning(sprintf(
        "%s 's processing status is not success. \nCause: %s", file$file_main,
        file$processing_status
      ))
      next
    }
    message(paste0("Analyzing: ", path, file$file_main))
    df <- read.csv(paste0(path, file$file_main),
      row.names = "date"
    ) %>% dplyr::select(-access_date)

    freq <- file$frequency

    ##
    # TODO: убрать костыль
    ##
    if (file$file_main == "exchangerate.csv") freq <- 365

    if (is.na(freq)) {
      warning(
        sprintf("%s 's frequency is unknown. Will not be included in the report table.", file$file_main)
      )
      next()
    }

    start_date <- as.numeric(strsplit(rownames(df)[1], "-")[[1]])
    end_date <- as.numeric(strsplit(rownames(df)[nrow(df)], "-")[[1]])
    df_ts <- ts(df, start = c(start_date[1], start_date[2]), frequency = freq)
    row.names(df_ts) <- rownames(df)

    # Working with TSs in file
    ts_names <- colnames(df_ts)
    ts_count <- length(ts_names)
    for (ts_num in 1:ts_count) {
      cur_ts <- df_ts[, ts_num]
      cur_ts_log <- data.frame(
        "ts_name" = NA, "comment" = NA,
        "freq" = NA,
        "file_name" = NA, "numeric_values" = NA,
        "obs_num" = NA,
        "first_obs_date" = NA,
        "last_obs_date" = NA, "NA_cnt" = NA,
        "omitted_dates_cnt" = NA, "corr_t_t.1" = NA,
        "corr_t_t.freq" = NA, "MAPE_naive" = NA, "MAPE_Snaive" = NA,
        "MAPE_auto_ets" = NA
      )
      cur_ts_log["ts_name"] <- ts_names[ts_num]
      cur_ts_log["comment"] <- file$comment
      cur_ts_log["freq"] <- freq
      cur_ts_log["file_name"] <- file$file_main
      cur_ts_log["numeric_values"] <- is.numeric(cur_ts)
      cur_ts_log["obs_num"] <- length(cur_ts)
      cur_ts_log["first_obs_date"] <- as.Date(rownames(df)[1],
        format = "%Y-%m-%d"
      )
      cur_ts_log["last_obs_date"] <- as.Date(rownames(df)[nrow(df)],
        format = "%Y-%m-%d"
      )
      cur_ts_log["NA_cnt"] <- sum(is.na(cur_ts))
      cur_ts_log["omitted_dates_cnt"] <- 0
      time_arr <- time(cur_ts)
      delta <- 1 / freq
      for (i in 2:length(time_arr)) {
        cur_ts_log["omitted_dates_cnt"] <- cur_ts_log["omitted_dates_cnt"]
        +as.integer((time_arr[i] - time_arr[i - 1]) / delta) - 1
      }

      ts_is_ok <- cur_ts_log["numeric_values"] & cur_ts_log["obs_num"] >= 3 * freq &
        cur_ts_log["NA_cnt"] == 0

      if (ts_is_ok) {
        cur_ts_t <- cur_ts[2:length(cur_ts)]
        cur_ts_log["corr_t_t.1"] <- cor(cur_ts[1:(length(cur_ts) - 1)], cur_ts_t)

        cur_ts_log["MAPE_naive"] <- mean(
          na.omit(
            abs(diff(cur_ts) / cur_ts[1:(length(cur_ts) - 1)]) * 100
          )
        )

        cur_ts_t <- cur_ts[(freq + 1):length(cur_ts)]
        cur_ts_log["corr_t_t.freq"] <- cor(cur_ts[1:(length(cur_ts) - freq)], cur_ts_t)

        cur_ts_log["MAPE_Snaive"] <- mean(
          na.omit(
            abs(diff(cur_ts, lag = freq) / cur_ts[1:(length(cur_ts) - freq)]) * 100
          )
        )

        etsfit <- forecast::ets(cur_ts)
        cur_ts_log["MAPE_auto_ets"] <- forecast::accuracy(etsfit)[5]
      }

      # add current ts report to report table
      result_table <- rbind(result_table, cur_ts_log)
    }
  }
  return(result_table)
}

#' @title Produces warnings table
#' @description Raises warnings and produces a table of suspicious metrics' values.
#' @details Use warnings() to see them.
#' @param report_table downloaded series' metadata
#' @return data.frame
#' @export
#' @examples
#' \donttest{
#' report_table <- make_report(download_log_new, path_day)
#' warnings_table <- raise_warnings(report_table)
#' }
raise_warnings <- function(report_table, valid_freqs = c(4, 12, 7, 365)) {
  warnings_table <- data.frame("ts_name" = character(), "problem" = character())
  template <- data.frame("ts_name" = NA, "problem" = NA)
  for (ts_num in 1:nrow(report_table)) {
    ts <- report_table[ts_num, ]
    if (!ts["freq"] %in% valid_freqs) {
      warning <- template
      warning_message <- sprintf(
        "%s has suspicious frequency value = %s, expected one of the list (4,12,7,365)",
        ts["ts_name"], ts["freq"]
      )
      warning["ts_name"] <- ts["ts_name"]
      warning["problem"] <- warning_message
      warnings_table <- rbind(warnings_table, warning)
    }

    if (!is.na(!ts["numeric_values"]) & !as.logical(ts["numeric_values"])) {
      warning <- template
      warning_message <- sprintf(
        "%s has non-numeric objects",
        ts["ts_name"], ts["numeric_values"]
      )
      warning["ts_name"] <- ts["ts_name"]
      warning["problem"] <- warning_message
      warnings_table <- rbind(warnings_table, warning)
    }

    if (!is.na(!ts["obs_num"]) & ts["obs_num"] < 3 * ts["freq"]) {
      warning <- template
      warning_message <- sprintf(
        "%s has low number of observations = %s, whereas at least %s is expected",
        ts["ts_name"], ts["obs_num"], 3 * ts["freq"]
      )
      warning["ts_name"] <- ts["ts_name"]
      warning["problem"] <- warning_message
      warnings_table <- rbind(warnings_table, warning)
    }

    freq <- ts["freq"]
    last_obs_date <- as.Date(ts[1, ]["last_obs_date"][1, 1])

    diff_in_days <- difftime(Sys.Date(), last_obs_date, units = "days")
    diff_in_weeks <- difftime(Sys.Date(), last_obs_date, units = "weeks")
    diff_in_years <- as.double(diff_in_days) / 365
    diff_in_quarters <- floor(diff_in_years * 4)
    diff_in_months <- floor(diff_in_years * 12)

    diff_in_freq <- NA
    delta <- NA
    if (freq == 4) {
      diff_in_freq <- as.double(diff_in_quarters)
      delta <- 1
    }
    if (freq == 12) {
      diff_in_freq <- as.double(diff_in_months)
      delta <- 2
    }
    if (freq == 7) {
      diff_in_freq <- as.double(diff_in_weeks)
      delta <- 1
    }
    if (freq == 365) {
      diff_in_freq <- as.double(diff_in_days)
      delta <- 7
    }

    if (!is.na(diff_in_freq) & diff_in_freq > 2 * delta) {
      warning <- template
      warning_message <- sprintf(
        "%s's last observation dated %s",
        ts["ts_name"], last_obs_date
      )
      warning["ts_name"] <- ts["ts_name"]
      warning["problem"] <- warning_message
      warnings_table <- rbind(warnings_table, warning)
    }

    if (!is.na(ts["NA_cnt"]) & ts["NA_cnt"] > 0) {
      warning <- template
      warning_message <- sprintf(
        "%s has %s NAs",
        ts["ts_name"], ts["NA_cnt"]
      )
      warning["ts_name"] <- ts["ts_name"]
      warning["problem"] <- warning_message
      warnings_table <- rbind(warnings_table, warning)
    }

    if (!is.na(ts["omitted_dates_cnt"]) & ts["omitted_dates_cnt"] > 0) {
      warning <- template
      warning_message <- sprintf(
        "%s has %s ommited dates",
        ts["ts_name"], ts["omitted_dates_cnt"]
      )
      warning["ts_name"] <- ts["ts_name"]
      warning["problem"] <- warning_message
      warnings_table <- rbind(warnings_table, warning)
    }

    if (!is.na(ts["corr_t_t.1"]) & (ts["corr_t_t.1"] < 0.2)) {
      warning <- template
      warning_message <- sprintf(
        "%s has suspicious corr(yt, yt-1) value = %s",
        ts["ts_name"], ts["corr_t_t.1"]
      )
      warning["ts_name"] <- ts["ts_name"]
      warning["problem"] <- warning_message
      warnings_table <- rbind(warnings_table, warning)
    }

    if (!is.na(ts["corr_t_t.freq"]) & (ts["corr_t_t.freq"] < 0.2)) {
      warning <- template
      warning_message <- sprintf(
        "%s has suspicious corr(yt, yt-%s) value = %s",
        ts["ts_name"], freq, ts["corr_t_t.freq"]
      )
      warning["ts_name"] <- ts["ts_name"]
      warning["problem"] <- warning_message
      warnings_table <- rbind(warnings_table, warning)
    }

    if (!is.na(ts["MAPE_naive"]) & (ts["MAPE_naive"] > 30)) {
      warning <- template
      warning_message <- sprintf(
        "%s has suspicious naive forecast's MAPE = %s (30 maximum valueis expected)",
        ts["ts_name"], ts["MAPE_naive"]
      )
      warning["ts_name"] <- ts["ts_name"]
      warning["problem"] <- warning_message
      warnings_table <- rbind(warnings_table, warning)
    }

    if (!is.na(ts["MAPE_Snaive"]) & (ts["MAPE_Snaive"] > 30)) {
      warning <- template
      warning_message <- sprintf(
        "%s has suspicious seasonal naive forecast's MAPE = %s (30 maximum value is expected)",
        ts["ts_name"], ts["MAPE_Snaive"]
      )
      warning["ts_name"] <- ts["ts_name"]
      warning["problem"] <- warning_message
      warnings_table <- rbind(warnings_table, warning)
    }

    if (!is.na(ts["MAPE_auto_ets"]) & (ts["MAPE_auto_ets"] > 30)) {
      warning <- template
      warning_message <- sprintf(
        "%s has suspicious automatic ets forecast's MAPE = %s (30 maximum value is expected)",
        ts["ts_name"], ts["MAPE_Snaive"]
      )
      warning["ts_name"] <- ts["ts_name"]
      warning["problem"] <- warning_message
      warnings_table <- rbind(warnings_table, warning)
    }
  }


  # raise warnings if needed
  if (nrow(warnings_table) != 0) {
    for (warning_num in 1:nrow(warnings_table)) {
      warning <- warnings_table[warning_num, 2]
      warning(warning)
    }
  }
  return(warnings_table)
}
