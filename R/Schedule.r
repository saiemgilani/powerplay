# NHL API
#
# Documenting the publicly accessible portions of the NHL API.
#
# OpenAPI spec version: 1.0.0
# 
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' Schedule Class
#'
#' @field copyright 
#' @field totalItems 
#' @field totalEvents 
#' @field totalGames 
#' @field totalMatches 
#' @field wait 
#' @field dates 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Schedule <- R6::R6Class(
  'Schedule',
  public = list(
    `copyright` = NULL,
    `totalItems` = NULL,
    `totalEvents` = NULL,
    `totalGames` = NULL,
    `totalMatches` = NULL,
    `wait` = NULL,
    `dates` = NULL,
    initialize = function(`copyright`, `totalItems`, `totalEvents`, `totalGames`, `totalMatches`, `wait`, `dates`){
      if (!missing(`copyright`)) {
        stopifnot(is.character(`copyright`), length(`copyright`) == 1)
        self$`copyright` <- `copyright`
      }
      if (!missing(`totalItems`)) {
        stopifnot(is.numeric(`totalItems`), length(`totalItems`) == 1)
        stopifnot(R6::is.R6(`totalItems`))
        self$`totalItems` <- `totalItems`
      }
      if (!missing(`totalEvents`)) {
        stopifnot(is.numeric(`totalEvents`), length(`totalEvents`) == 1)
        stopifnot(R6::is.R6(`totalEvents`))
        self$`totalEvents` <- `totalEvents`
      }
      if (!missing(`totalGames`)) {
        stopifnot(is.numeric(`totalGames`), length(`totalGames`) == 1)
        stopifnot(R6::is.R6(`totalGames`))
        self$`totalGames` <- `totalGames`
      }
      if (!missing(`totalMatches`)) {
        stopifnot(is.numeric(`totalMatches`), length(`totalMatches`) == 1)
        stopifnot(R6::is.R6(`totalMatches`))
        self$`totalMatches` <- `totalMatches`
      }
      if (!missing(`wait`)) {
        stopifnot(is.numeric(`wait`), length(`wait`) == 1)
        stopifnot(R6::is.R6(`wait`))
        self$`wait` <- `wait`
      }
      if (!missing(`dates`)) {
        stopifnot(is.list(`dates`), length(`dates`) != 0)
        lapply(`dates`, function(x) stopifnot(R6::is.R6(x)))
        self$`dates` <- `dates`
      }
    },
    toJSON = function() {
      ScheduleObject <- list()
      if (!is.null(self$`copyright`)) {
        ScheduleObject[['copyright']] <- self$`copyright`
      }
      if (!is.null(self$`totalItems`)) {
        ScheduleObject[['totalItems']] <- self$`totalItems`$toJSON()
      }
      if (!is.null(self$`totalEvents`)) {
        ScheduleObject[['totalEvents']] <- self$`totalEvents`$toJSON()
      }
      if (!is.null(self$`totalGames`)) {
        ScheduleObject[['totalGames']] <- self$`totalGames`$toJSON()
      }
      if (!is.null(self$`totalMatches`)) {
        ScheduleObject[['totalMatches']] <- self$`totalMatches`$toJSON()
      }
      if (!is.null(self$`wait`)) {
        ScheduleObject[['wait']] <- self$`wait`$toJSON()
      }
      if (!is.null(self$`dates`)) {
        ScheduleObject[['dates']] <- lapply(self$`dates`, function(x) x$toJSON())
      }

      ScheduleObject
    },
    fromJSON = function(ScheduleJson) {
      ScheduleObject <- jsonlite::fromJSON(ScheduleJson)
      if (!is.null(ScheduleObject$`copyright`)) {
        self$`copyright` <- ScheduleObject$`copyright`
      }
      if (!is.null(ScheduleObject$`totalItems`)) {
        totalItemsObject <- BigDecimal$new()
        totalItemsObject$fromJSON(jsonlite::toJSON(ScheduleObject$totalItems, auto_unbox = TRUE))
        self$`totalItems` <- totalItemsObject
      }
      if (!is.null(ScheduleObject$`totalEvents`)) {
        totalEventsObject <- BigDecimal$new()
        totalEventsObject$fromJSON(jsonlite::toJSON(ScheduleObject$totalEvents, auto_unbox = TRUE))
        self$`totalEvents` <- totalEventsObject
      }
      if (!is.null(ScheduleObject$`totalGames`)) {
        totalGamesObject <- BigDecimal$new()
        totalGamesObject$fromJSON(jsonlite::toJSON(ScheduleObject$totalGames, auto_unbox = TRUE))
        self$`totalGames` <- totalGamesObject
      }
      if (!is.null(ScheduleObject$`totalMatches`)) {
        totalMatchesObject <- BigDecimal$new()
        totalMatchesObject$fromJSON(jsonlite::toJSON(ScheduleObject$totalMatches, auto_unbox = TRUE))
        self$`totalMatches` <- totalMatchesObject
      }
      if (!is.null(ScheduleObject$`wait`)) {
        waitObject <- BigDecimal$new()
        waitObject$fromJSON(jsonlite::toJSON(ScheduleObject$wait, auto_unbox = TRUE))
        self$`wait` <- waitObject
      }
      if (!is.null(ScheduleObject$`dates`)) {
        self$`dates` <- lapply(ScheduleObject$`dates`, function(x) {
          datesObject <- ScheduleDay$new()
          datesObject$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
          datesObject
        })
      }
    },
    toJSONString = function() {
       sprintf(
        '{
           "copyright": %s,
           "totalItems": %s,
           "totalEvents": %s,
           "totalGames": %s,
           "totalMatches": %s,
           "wait": %s,
           "dates": [%s]
        }',
        self$`copyright`,
        self$`totalItems`$toJSON(),
        self$`totalEvents`$toJSON(),
        self$`totalGames`$toJSON(),
        self$`totalMatches`$toJSON(),
        self$`wait`$toJSON(),
        lapply(self$`dates`, function(x) paste(x$toJSON(), sep=","))
      )
    },
    fromJSONString = function(ScheduleJson) {
      ScheduleObject <- jsonlite::fromJSON(ScheduleJson)
      self$`copyright` <- ScheduleObject$`copyright`
      BigDecimalObject <- BigDecimal$new()
      self$`totalItems` <- BigDecimalObject$fromJSON(jsonlite::toJSON(ScheduleObject$totalItems, auto_unbox = TRUE))
      BigDecimalObject <- BigDecimal$new()
      self$`totalEvents` <- BigDecimalObject$fromJSON(jsonlite::toJSON(ScheduleObject$totalEvents, auto_unbox = TRUE))
      BigDecimalObject <- BigDecimal$new()
      self$`totalGames` <- BigDecimalObject$fromJSON(jsonlite::toJSON(ScheduleObject$totalGames, auto_unbox = TRUE))
      BigDecimalObject <- BigDecimal$new()
      self$`totalMatches` <- BigDecimalObject$fromJSON(jsonlite::toJSON(ScheduleObject$totalMatches, auto_unbox = TRUE))
      BigDecimalObject <- BigDecimal$new()
      self$`wait` <- BigDecimalObject$fromJSON(jsonlite::toJSON(ScheduleObject$wait, auto_unbox = TRUE))
      self$`dates` <- lapply(ScheduleObject$`dates`, function(x) ScheduleDay$new()$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE)))
    }
  )
)
