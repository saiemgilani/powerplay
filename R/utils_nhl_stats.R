#' @title year to season (XXXX -> XXXX-YY)
#' @param year Four digit year (XXXX)
#' @importFrom dplyr mutate filter select left_join
#' @importFrom stringr str_detect
#' @importFrom tidyr everything
year_to_season <- function(year){
  first_year <- substr(year,3,4)
  next_year <- as.numeric(first_year)+1
  next_year <- dplyr::case_when(
    next_year <10 & first_year > 0 ~ glue::glue("0{next_year}"),
    first_year == 99 ~ "00",
    TRUE ~ as.character(next_year))
  return(glue::glue("{year}-{next_year}"))
}
