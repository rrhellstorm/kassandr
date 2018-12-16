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

