# NHL API
#
# Documenting the publicly accessible portions of the NHL API.
#
# OpenAPI spec version: 1.0.0
# 
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' GameGameDataTeams Class
#'
#' @field away 
#' @field home 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
GameGameDataTeams <- R6::R6Class(
  'GameGameDataTeams',
  public = list(
    `away` = NULL,
    `home` = NULL,
    initialize = function(`away`, `home`){
      if (!missing(`away`)) {
        stopifnot(R6::is.R6(`away`))
        self$`away` <- `away`
      }
      if (!missing(`home`)) {
        stopifnot(R6::is.R6(`home`))
        self$`home` <- `home`
      }
    },
    toJSON = function() {
      GameGameDataTeamsObject <- list()
      if (!is.null(self$`away`)) {
        GameGameDataTeamsObject[['away']] <- self$`away`$toJSON()
      }
      if (!is.null(self$`home`)) {
        GameGameDataTeamsObject[['home']] <- self$`home`$toJSON()
      }

      GameGameDataTeamsObject
    },
    fromJSON = function(GameGameDataTeamsJson) {
      GameGameDataTeamsObject <- jsonlite::fromJSON(GameGameDataTeamsJson)
      if (!is.null(GameGameDataTeamsObject$`away`)) {
        awayObject <- Team$new()
        awayObject$fromJSON(jsonlite::toJSON(GameGameDataTeamsObject$away, auto_unbox = TRUE))
        self$`away` <- awayObject
      }
      if (!is.null(GameGameDataTeamsObject$`home`)) {
        homeObject <- Team$new()
        homeObject$fromJSON(jsonlite::toJSON(GameGameDataTeamsObject$home, auto_unbox = TRUE))
        self$`home` <- homeObject
      }
    },
    toJSONString = function() {
       sprintf(
        '{
           "away": %s,
           "home": %s
        }',
        self$`away`$toJSON(),
        self$`home`$toJSON()
      )
    },
    fromJSONString = function(GameGameDataTeamsJson) {
      GameGameDataTeamsObject <- jsonlite::fromJSON(GameGameDataTeamsJson)
      TeamObject <- Team$new()
      self$`away` <- TeamObject$fromJSON(jsonlite::toJSON(GameGameDataTeamsObject$away, auto_unbox = TRUE))
      TeamObject <- Team$new()
      self$`home` <- TeamObject$fromJSON(jsonlite::toJSON(GameGameDataTeamsObject$home, auto_unbox = TRUE))
    }
  )
)
