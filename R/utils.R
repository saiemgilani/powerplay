.datatable.aware <- TRUE

#' Progressively
#'
#' This function helps add progress-reporting to any function - given function `f()` and progressor `p()`, it will return a new function that calls `f()` and then (on-exiting) will call `p()` after every iteration.
#'
#' This is inspired by purrr's `safely`, `quietly`, and `possibly` function decorators.
#'
#' @param f a function to add progressr functionality to.
#' @param p a progressor function as created by `progressr::progressor()`
#'
#' @return a function that does the same as `f` but it calls `p()` after iteration.
#'
progressively <- function(f, p = NULL){
  if(!is.null(p) && !inherits(p, "progressor")) stop("`p` must be a progressor function!")
  if(is.null(p)) p <- function(...) NULL
  force(f)

  function(...){
    on.exit(p("loading..."))
    f(...)
  }

}


#' @title
#' **Load .csv / .csv.gz file from a remote connection**
#' @description
#' This is a thin wrapper on data.table::fread
#' @param ... passed to data.table::fread
#' @importFrom data.table fread
#' @return a dataframe as created by [`data.table::fread()`]
csv_from_url <- function(...){
  data.table::fread(...)
}

.datatable.aware <- TRUE


#' @title
#' **Load .rds file from a remote connection**
#' @param url a character url
#' @return a dataframe as created by [`readRDS()`]
#' @importFrom data.table data.table setDT
#' @import rvest
rds_from_url <- function(url) {
  con <- url(url)
  on.exit(close(con))
  load <- try(readRDS(con), silent = TRUE)

  if (inherits(load, "try-error")) {
    warning(paste0("Failed to readRDS from <", url, ">"), call. = FALSE)
    return(data.table::data.table())
  }

  data.table::setDT(load)
  return(load)
}


# check if a package is installed
is_installed <- function(pkg) requireNamespace(pkg, quietly = TRUE)
# custom mode function from https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode/8189441
custom_mode <- function(x, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

#' @title
#' **Most Recent NHL Season**
#' @export
most_recent_nhl_season <- function() {
  dplyr::if_else(
    as.double(substr(Sys.Date(), 6, 7)) >= 10,
    as.double(substr(Sys.Date(), 1, 4))+1,
    as.double(substr(Sys.Date(), 1, 4))
  )
}


my_time <- function() strftime(Sys.time(), format = "%H:%M:%S")

#' Check Status function
#' @param res Response from API
#' @keywords Internal
#' @import rvest
#'
check_status <- function(res) {

  x = httr::status_code(res)

  if(x != 200) stop("The API returned an error", call. = FALSE)

}

#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' @import utils
utils::globalVariables(c("where"))


#' @keywords internal
"_PACKAGE"

#' @importFrom Rcpp getRcppVersion
#' @importFrom RcppParallel defaultNumThreads
NULL

`%c%` <- function(x,y){
  ifelse(!is.na(x),x,y)
}
