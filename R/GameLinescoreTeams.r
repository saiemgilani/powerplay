# NHL API
#
# Documenting the publicly accessible portions of the NHL API.
#
# OpenAPI spec version: 1.0.0
# 
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' GameLinescoreTeams Class
#'
#' @field home 
#' @field away 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
GameLinescoreTeams <- R6::R6Class(
  'GameLinescoreTeams',
  public = list(
    `home` = NULL,
    `away` = NULL,
    initialize = function(`home`, `away`){
      if (!missing(`home`)) {
        stopifnot(R6::is.R6(`home`))
        self$`home` <- `home`
      }
      if (!missing(`away`)) {
        stopifnot(R6::is.R6(`away`))
        self$`away` <- `away`
      }
    },
    toJSON = function() {
      GameLinescoreTeamsObject <- list()
      if (!is.null(self$`home`)) {
        GameLinescoreTeamsObject[['home']] <- self$`home`$toJSON()
      }
      if (!is.null(self$`away`)) {
        GameLinescoreTeamsObject[['away']] <- self$`away`$toJSON()
      }

      GameLinescoreTeamsObject
    },
    fromJSON = function(GameLinescoreTeamsJson) {
      GameLinescoreTeamsObject <- jsonlite::fromJSON(GameLinescoreTeamsJson)
      if (!is.null(GameLinescoreTeamsObject$`home`)) {
        homeObject <- GameLinescoreTeam$new()
        homeObject$fromJSON(jsonlite::toJSON(GameLinescoreTeamsObject$home, auto_unbox = TRUE))
        self$`home` <- homeObject
      }
      if (!is.null(GameLinescoreTeamsObject$`away`)) {
        awayObject <- GameLinescoreTeam$new()
        awayObject$fromJSON(jsonlite::toJSON(GameLinescoreTeamsObject$away, auto_unbox = TRUE))
        self$`away` <- awayObject
      }
    },
    toJSONString = function() {
       sprintf(
        '{
           "home": %s,
           "away": %s
        }',
        self$`home`$toJSON(),
        self$`away`$toJSON()
      )
    },
    fromJSONString = function(GameLinescoreTeamsJson) {
      GameLinescoreTeamsObject <- jsonlite::fromJSON(GameLinescoreTeamsJson)
      GameLinescoreTeamObject <- GameLinescoreTeam$new()
      self$`home` <- GameLinescoreTeamObject$fromJSON(jsonlite::toJSON(GameLinescoreTeamsObject$home, auto_unbox = TRUE))
      GameLinescoreTeamObject <- GameLinescoreTeam$new()
      self$`away` <- GameLinescoreTeamObject$fromJSON(jsonlite::toJSON(GameLinescoreTeamsObject$away, auto_unbox = TRUE))
    }
  )
)
