# NHL API
#
# Documenting the publicly accessible portions of the NHL API.
#
# OpenAPI spec version: 1.0.0
# 
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' GameLiveDataPlays Class
#'
#' @field allPlays 
#' @field scoringPlays 
#' @field penaltyPlays 
#' @field playsByPeriod 
#' @field currentPlay 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
GameLiveDataPlays <- R6::R6Class(
  'GameLiveDataPlays',
  public = list(
    allPlays = NULL,
    scoringPlays = NULL,
    penaltyPlays = NULL,
    playsByPeriod = NULL,
    currentPlay = NULL,
    initialize = function(allPlays, scoringPlays, penaltyPlays, playsByPeriod, currentPlay){
      if (!missing(allPlays)) {
        stopifnot(is.list(allPlays), length(allPlays) != 0)
        lapply(allPlays, function(x) stopifnot(R6::is.R6(x)))
        self$allPlays <- allPlays
      }
      if (!missing(scoringPlays)) {
        stopifnot(is.list(scoringPlays), length(scoringPlays) != 0)
        lapply(scoringPlays, function(x) stopifnot(R6::is.R6(x)))
        self$scoringPlays <- scoringPlays
      }
      if (!missing(penaltyPlays)) {
        stopifnot(is.list(penaltyPlays), length(penaltyPlays) != 0)
        lapply(penaltyPlays, function(x) stopifnot(R6::is.R6(x)))
        self$penaltyPlays <- penaltyPlays
      }
      if (!missing(playsByPeriod)) {
        stopifnot(is.list(playsByPeriod), length(playsByPeriod) != 0)
        lapply(playsByPeriod, function(x) stopifnot(R6::is.R6(x)))
        self$playsByPeriod <- playsByPeriod
      }
      if (!missing(currentPlay)) {
        stopifnot(R6::is.R6(currentPlay))
        self$currentPlay <- currentPlay
      }
    },
    toJSON = function() {
      GameLiveDataPlaysObject <- list()
      if (!is.null(self$allPlays)) {
        GameLiveDataPlaysObject[['allPlays']] <- lapply(self$allPlays, function(x) x$toJSON())
      }
      if (!is.null(self$scoringPlays)) {
        GameLiveDataPlaysObject[['scoringPlays']] <- lapply(self$scoringPlays, function(x) x$toJSON())
      }
      if (!is.null(self$penaltyPlays)) {
        GameLiveDataPlaysObject[['penaltyPlays']] <- lapply(self$penaltyPlays, function(x) x$toJSON())
      }
      if (!is.null(self$playsByPeriod)) {
        GameLiveDataPlaysObject[['playsByPeriod']] <- lapply(self$playsByPeriod, function(x) x$toJSON())
      }
      if (!is.null(self$currentPlay)) {
        GameLiveDataPlaysObject[['currentPlay']] <- self$currentPlay$toJSON()
      }

      GameLiveDataPlaysObject
    },
    fromJSON = function(GameLiveDataPlaysJson) {
      GameLiveDataPlaysObject <- jsonlite::fromJSON(GameLiveDataPlaysJson)
      if (!is.null(GameLiveDataPlaysObject$allPlays)) {
        self$allPlays <- lapply(GameLiveDataPlaysObject$allPlays, function(x) {
          allPlaysObject <- GamePlay$new()
          allPlaysObject$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
          allPlaysObject
        })
      }
      if (!is.null(GameLiveDataPlaysObject$scoringPlays)) {
        self$scoringPlays <- lapply(GameLiveDataPlaysObject$scoringPlays, function(x) {
          scoringPlaysObject <- BigDecimal$new()
          scoringPlaysObject$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
          scoringPlaysObject
        })
      }
      if (!is.null(GameLiveDataPlaysObject$penaltyPlays)) {
        self$penaltyPlays <- lapply(GameLiveDataPlaysObject$penaltyPlays, function(x) {
          penaltyPlaysObject <- BigDecimal$new()
          penaltyPlaysObject$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
          penaltyPlaysObject
        })
      }
      if (!is.null(GameLiveDataPlaysObject$playsByPeriod)) {
        self$playsByPeriod <- lapply(GameLiveDataPlaysObject$playsByPeriod, function(x) {
          playsByPeriodObject <- GameLiveDataPlaysPlaysByPeriod$new()
          playsByPeriodObject$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
          playsByPeriodObject
        })
      }
      if (!is.null(GameLiveDataPlaysObject$currentPlay)) {
        currentPlayObject <- GamePlay$new()
        currentPlayObject$fromJSON(jsonlite::toJSON(GameLiveDataPlaysObject$currentPlay, auto_unbox = TRUE))
        self$currentPlay <- currentPlayObject
      }
    },
    toJSONString = function() {
       sprintf(
        '{
           "allPlays": [%s],
           "scoringPlays": [%s],
           "penaltyPlays": [%s],
           "playsByPeriod": [%s],
           "currentPlay": %s
        }',
        lapply(self$allPlays, function(x) paste(x$toJSON(), sep=",")),
        lapply(self$scoringPlays, function(x) paste(x$toJSON(), sep=",")),
        lapply(self$penaltyPlays, function(x) paste(x$toJSON(), sep=",")),
        lapply(self$playsByPeriod, function(x) paste(x$toJSON(), sep=",")),
        self$currentPlay$toJSON()
      )
    },
    fromJSONString = function(GameLiveDataPlaysJson) {
      GameLiveDataPlaysObject <- jsonlite::fromJSON(GameLiveDataPlaysJson)
      self$allPlays <- lapply(GameLiveDataPlaysObject$allPlays, function(x) GamePlay$new()$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE)))
      self$scoringPlays <- lapply(GameLiveDataPlaysObject$scoringPlays, function(x) BigDecimal$new()$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE)))
      self$penaltyPlays <- lapply(GameLiveDataPlaysObject$penaltyPlays, function(x) BigDecimal$new()$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE)))
      self$playsByPeriod <- lapply(GameLiveDataPlaysObject$playsByPeriod, function(x) GameLiveDataPlaysPlaysByPeriod$new()$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE)))
      GamePlayObject <- GamePlay$new()
      self$currentPlay <- GamePlayObject$fromJSON(jsonlite::toJSON(GameLiveDataPlaysObject$currentPlay, auto_unbox = TRUE))
    }
  )
)