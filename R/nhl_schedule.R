#' @title NHL Schedule
#' @description Returns NHL Schedule data
#' @param season NHL Season
#' @param day Date
#' @return Returns a named list of data frames: player, stats
#' @keywords NHL Player stats
#' @import rvest
#' @importFrom rlang .data
#' @importFrom lubridate ms period_to_seconds
#' @importFrom jsonlite fromJSON toJSON read_json
#' @importFrom dplyr mutate filter select rename bind_cols bind_rows tibble
#' @importFrom tidyr unnest unnest_wider everything
#' @importFrom janitor clean_names
#' @export
#' @examples 
#' \donttest{
#'   nhl_schedule(season = 2020)
#' }
nhl_schedule <- function(season = NULL, day = as.Date(Sys.Date(), "%Y-%m-%d")){
  
  
  if(is.null(season)){
    
    # scrape day's games
    
    url <- glue::glue("https://statsapi.web.nhl.com/api/v1/schedule?date={day}")
    
    site <- jsonlite::read_json(url)
    
    if(site$totalGames == 0){
      message(glue::glue("No NHL games found on {day}"))
    }
    
  } else {
    
    # scrape season's games
    
    if(season == 2020){
      # searching the nhl api for games between Sep 1 2019 & Sep 30th 2020
      # what a stupid season
      url <- glue::glue("https://statsapi.web.nhl.com/api/v1/schedule?startDate={season-1}-09-01&endDate={season}-09-30")
    } else {
      # searching the nhl api for games between Sep 1 & July 5
      url <- glue::glue("https://statsapi.web.nhl.com/api/v1/schedule?startDate={season-1}-09-01&endDate={season}-07-31")
    }
    
    site <- jsonlite::read_json(url)
    
  }
  
  if(site$totalGames == 0) {
    game_id_list <- NULL
  } else {
    game_id_list <- site$dates %>%
      dplyr::tibble() %>%
      tidyr::unnest_wider(1) %>%
      dplyr::select(.data$date, .data$games) %>%
      tidyr::unnest_longer(.data$games) %>%
      tidyr::unnest_wider(.data$games) %>%
      dplyr::select(.data$date, .data$gamePk, .data$season, .data$teams) %>%
      tidyr::unnest_wider(.data$teams) %>%
      tidyr::unnest_wider(.data$away) %>%
      tidyr::unnest_wider(.data$team) %>%
      dplyr::rename(
        game_id = .data$gamePk,
        season_full = .data$season,
        away_name = .data$name,
        away_final_score = .data$score
      ) %>%
      dplyr::select(-.data$leagueRecord, -.data$id, -.data$link) %>%
      tidyr::unnest_wider(.data$home) %>%
      tidyr::unnest_wider(.data$team) %>%
      dplyr::rename(
        home_name = .data$name,
        home_final_score = .data$score
      ) %>%
      dplyr::select(
        .data$game_id, .data$season_full, .data$date, 
        .data$home_name, .data$away_name, 
        .data$home_final_score, .data$away_final_score)
    
    game_id_list$game_type <- dplyr::case_when(
      substr(game_id_list$game_id, 6, 6) == 1 ~ "PRE",
      substr(game_id_list$game_id, 6, 6) == 2 ~ "REG",
      substr(game_id_list$game_id, 6, 6) == 3 ~ "POST",
      substr(game_id_list$game_id, 6, 6) == 4 ~ "ALLSTAR"
    )
    
    game_id_list <- dplyr::filter(game_id_list,
                                  .data$game_type == "REG" | .data$game_type == "POST")
    
    # make sure we're only pulling for correct season by using
    # the season code in the game_id
    
    if(!is.null(season)) {
      game_id_list <- game_id_list %>%
        dplyr::filter(
          substr(.data$game_id, 1, 4) == (as.numeric(season) - 1)
        )
    }
    
  }
  
  return(game_id_list)
}