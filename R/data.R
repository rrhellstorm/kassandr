#' Tibble of converters and parsers to download data
#'
#' Tibble of converters and parsers to download data.
#' To update watchdog your may proceed in the following steps:
#'
#' 1. Set the working directory to the package folder.
#'
#' 2. Update the watchdog dataset in memory.
#'
#' 3. usethis::use_data(watchdog)
#'
#' 4. devtools::check()
#'
#' 5. upload package on github
#'
#' @format A data frame with 7 columns
#' \describe{
#'   \item{url}{url to download and parse, may be empty}
#'   \item{file_raw}{name of the raw file after download, may be empty}
#'   \item{file_main}{name of the main file after parsing, usually csv}
#'   \item{processing}{name of the processing function}
#'   \item{univariate}{TRUE for univariate and FALSE for multivariate}
#'   \item{frequency}{observations per year}
#'   \item{comment}{comment in free form}
#' }
"watchdog"
