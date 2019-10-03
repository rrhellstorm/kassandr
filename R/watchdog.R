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



#' Download all statistics
#'
#' Download all statistics
#'
#' Download all statistics
#'
#' @param path path to raw data folde
#' @param watchdog watchdog file
#' Structure of watchdog:
#' url - url of file to download, may be NA
#' file_raw - name for local raw file
#' file_main - name for local processed file
#' processing - name of processing function
#' univariate - TRUE/FALSE
#' frequency - 4/12/etc
#' comment - self explanatory :)
#' @param access_date access date
#' @param method for downloading files, passed to `download.file()`: "curl", "wget", "libcurl", "auto", "internal", "wininet"
#' @param extra options for downloading files, passed to `download.file()`: used for "wget" and "curl" methods
#' @return downloads log
#' @export
#' @examples
#' path = tempdir()
#' mini_watchdog = tibble::tibble(url = "http://www.gks.ru/free_doc/new_site/prices/potr/I_ipc.xlsx",
#'     file_raw = "i_ipc.xlsx", file_main = "i_ipc.csv", processing = "convert_i_ipc_xlsx")
#' download_log_new = download_statistics(path, mini_watchdog)
download_statistics = function(path, watchdog, access_date = Sys.Date(), method = "curl", extra = "-L") {
  download_log = watchdog %>% dplyr::mutate(access_date = access_date, download_status = NA, processing_status = NA, hash_raw = NA, hash_main = NA)

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
      attempt = try(utils::download.file(url = url, destfile = file_raw, method = method, extra = extra))
      if ("try-error" %in% class(attempt)) {
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
    if ("try-error" %in% class(data_processed)) {
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



