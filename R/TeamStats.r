# NHL API
#
# Documenting the publicly accessible portions of the NHL API.
#
# OpenAPI spec version: 1.0.0
# 
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' TeamStats Class
#'
#' @field copyright 
#' @field stats 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
TeamStats <- R6::R6Class(
  'TeamStats',
  public = list(
    `copyright` = NULL,
    `stats` = NULL,
    initialize = function(`copyright`, `stats`){
      if (!missing(`copyright`)) {
        stopifnot(is.character(`copyright`), length(`copyright`) == 1)
        self$`copyright` <- `copyright`
      }
      if (!missing(`stats`)) {
        stopifnot(is.list(`stats`), length(`stats`) != 0)
        lapply(`stats`, function(x) stopifnot(R6::is.R6(x)))
        self$`stats` <- `stats`
      }
    },
    toJSON = function() {
      TeamStatsObject <- list()
      if (!is.null(self$`copyright`)) {
        TeamStatsObject[['copyright']] <- self$`copyright`
      }
      if (!is.null(self$`stats`)) {
        TeamStatsObject[['stats']] <- lapply(self$`stats`, function(x) x$toJSON())
      }

      TeamStatsObject
    },
    fromJSON = function(TeamStatsJson) {
      TeamStatsObject <- jsonlite::fromJSON(TeamStatsJson)
      if (!is.null(TeamStatsObject$`copyright`)) {
        self$`copyright` <- TeamStatsObject$`copyright`
      }
      if (!is.null(TeamStatsObject$`stats`)) {
        self$`stats` <- lapply(TeamStatsObject$`stats`, function(x) {
          statsObject <- TeamStatsStats$new()
          statsObject$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
          statsObject
        })
      }
    },
    toJSONString = function() {
       sprintf(
        '{
           "copyright": %s,
           "stats": [%s]
        }',
        self$`copyright`,
        lapply(self$`stats`, function(x) paste(x$toJSON(), sep=","))
      )
    },
    fromJSONString = function(TeamStatsJson) {
      TeamStatsObject <- jsonlite::fromJSON(TeamStatsJson)
      self$`copyright` <- TeamStatsObject$`copyright`
      self$`stats` <- lapply(TeamStatsObject$`stats`, function(x) TeamStatsStats$new()$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE)))
    }
  )
)
