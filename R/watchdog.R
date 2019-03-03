#' Extracts one line from watchdog file given url
#'
#' Extracts one line from watchdog file given url
#'
#' Extracts one line from watchdog file given url
#'
#' @param source_url url of guarded file
#' @param watchdog data frame with url column at least
#' @return one line from watchdog data frame
#' @export
#' @examples
#' watchdog_demo = dplyr::tibble(url = c("a", "b"), x = c(1, 2))
#' get_watchdog_line("a", watchdog_demo)
get_watchdog_line = function(source_url, watchdog) {
  if (!(source_url %in% watchdog$url)) {
    stop("The file ", source_url, " is not guarded by watchdog :)")
  }
  watchdog_line = dplyr::filter(watchdog, url == source_url)
  return(watchdog_line)  
}

#' Gets last version full path from watchdog file
#'
#' Gets last version full path from watchdog file
#'
#' Gets last version full path from watchdog file
#'
#' @param source_url url of guarded file
#' @param watchdog data frame with url, file and last_download columns at least
#' @return path to last downloaded version
#' @export
#' @examples
#' watchdog_demo = dplyr::tibble(url = c("a", "b"), file = c("xxx.xls", "yyy.xlsx"), 
#'   last_download = c("2011-11-11", "2010-10-10"))
#' get_last_version_path("a", watchdog_demo)
get_last_version_path = function(source_url, watchdog) {
  watchdog_line = get_watchdog_line(source_url, watchdog)
  download_date = watchdog_line$last_download
  file_name = watchdog_line$file
  last_version_path = paste0("../raw/", download_date, "/", file_name)
  return(last_version_path)
}

#' Replace file extension by specified ending
#'
#' Replace file extension by specified ending
#'
#' Replace file extension by specified ending
#' Old extension is removed and "_converted.csv" is added
#'
#' @param filename name of file with extension
#' @param new_ext string, new ending of full file name
#' @return new file name
#' @export
#' @examples
#' replace_extension("../test/demo.xlsx")
replace_extension = function(filename, new_ext = "_converted.csv") {
  new_filename = paste0(tools::file_path_sans_ext(filename), new_ext)
  return(new_filename)
}


#' Gets last version date from watchdog file
#'
#' Gets last version date from watchdog file
#'
#' Gets last version date from watchdog file
#'
#' @param source_url url of guarded file
#' @param watchdog data frame with url and last_download columns at least
#' @return date of last downloaded version
#' @export
#' @examples
#' watchdog_demo = dplyr::tibble(url = c("a", "b"), file = c("xxx.xls", "yyy.xlsx"), 
#'   last_download = c("2011-11-11", "2010-10-10"))
#' get_last_version_download_date("a", watchdog_demo)
get_last_version_download_date = function(source_url, watchdog) {
  watchdog_line = get_watchdog_line(source_url, watchdog)
  download_date = watchdog_line$last_download
  return(download_date)  
}

#' Download files guarded by watchdog
#'
#' Download files guarded by watchdog
#'
#' Download files guarded by watchdog
#'
#' @param raw_data_folder path to raw data folde
#' @param watchdog_file name of watchdog file
#' @return updated version of watchdog file
#' @export
#' @examples
#' raw_data_folder = "../raw/"
#' # new_watchdog = download_watchdog_files(raw_data_folder)
download_watchdog_files = function(raw_data_folder, watchdog_file = "watchdog.csv") {
  watchdog = rio::import(paste0(raw_data_folder, "/", watchdog_file))
  today = as.character(lubridate::today())
  today_folder = paste0(raw_data_folder, "/", today)
                         
  new_watchdog = watchdog 
                         
  for (file_no in 1:nrow(watchdog)) {
    url = watchdog$url[file_no]
    md5 = watchdog$hash[file_no]
    filename = watchdog$file[file_no]
                           
    tempfile = tempfile()
    attempt = try(utils::download.file(url = url, destfile = tempfile, method = "curl"))
    new_watchdog$last_access[file_no] = today
                           
    if (class(attempt) == "try-error") {
      # ошибка при скачивании: запомним её
      new_watchdog$last_status[file_no] = as.character(attempt)
    } else {
      new_md5 = digest::digest(file = tempfile)
        if (md5 != new_md5) {
          if (!dir.exists(today_folder)) {
            dir.create(today_folder)
          }
          file.copy(from = tempfile, to = paste0(today_folder, "/", filename))
          new_watchdog$hash[file_no] = new_md5
          new_watchdog$last_download[file_no] = today
          new_watchdog$last_status[file_no] = "successful download"
        } else {
          new_watchdog$last_status[file_no] = "no changes in md5"
        }
    }
  }
  return(new_watchdog)
}


#' Download all statistics
#'
#' Download all statistics
#'
#' Download all statistics
#'
#' @param path path to raw data folde
#' @param watchdog watchdog file
#' @param access_date access date
#' @return downloads log
#' @export
#' @examples
#' path = "../raw/"
#' # download_log_new = download_statistics(path, watchdog)
download_statistics = function(path, watchdog, access_date = Sys.Date()) {
  download_log = watchdog %>% mutate(access_date = access_date, download_status = NA, processing_status = NA, hash_raw = NA, hash_main = NA)
  
  today_folder = paste0(path, access_date, "/")
  if (!dir.exists(today_folder)) {
    dir.create(today_folder)
  }
  
  # download stage
  for (file_no in 1:nrow(download_log)) {
    url = download_log$url[file_no]
    if (!is.na(url)) {
      file_raw = paste0(today_folder, download_log$file_raw[file_no])
      message("Downloading ", url, ".")
      attempt = try(utils::download.file(url = url, destfile = file_raw, method = "curl"))
      if (class(attempt) == "try-error") {
        # ошибка при скачивании: запомним её
        download_log$access_status[file_no] = as.character(attempt)
      } else {
        download_log$hash_raw[file_no] = digest::digest(file = file_raw)
        download_log$download_status[file_no] = "success"
      }
    }
  }

  # processing stage
  for (file_no in 1:nrow(download_log)) {
    url = download_log$url[file_no]
    processing = download_log$processing[file_no]
    if (is.na(url)) {
      data_processed = try(do.call(processing, list(access_date)))
    } else {
      file_raw = paste0(today_folder, download_log$file_raw[file_no])
      data_processed = try(do.call(processing, list(file_raw, access_date)))
    }
    if (class(data_processed) == "try-error") {
      # ошибка при обработке: запомним её
      download_log$processing_status[file_no] = as.character(data_processed)
    } else {
      download_log$processing_status[file_no] = "success"
      file_main = paste0(today_folder, download_log$file_main[file_no])
      rio::export(data_processed, file_main)
      download_log$hash_main[file_no] = digest::digest(file = file_main)
    }
  }
  return(download_log)
}



