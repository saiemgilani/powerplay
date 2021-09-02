# NHL API
#
# Documenting the publicly accessible portions of the NHL API.
#
# OpenAPI spec version: 1.0.0
# 
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' GamePlayPlayers Class
#'
#' @field player 
#' @field playerType 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
GamePlayPlayers <- R6::R6Class(
  'GamePlayPlayers',
  public = list(
    `player` = NULL,
    `playerType` = NULL,
    initialize = function(`player`, `playerType`){
      if (!missing(`player`)) {
        stopifnot(R6::is.R6(`player`))
        self$`player` <- `player`
      }
      if (!missing(`playerType`)) {
        stopifnot(is.character(`playerType`), length(`playerType`) == 1)
        self$`playerType` <- `playerType`
      }
    },
    toJSON = function() {
      GamePlayPlayersObject <- list()
      if (!is.null(self$`player`)) {
        GamePlayPlayersObject[['player']] <- self$`player`$toJSON()
      }
      if (!is.null(self$`playerType`)) {
        GamePlayPlayersObject[['playerType']] <- self$`playerType`
      }

      GamePlayPlayersObject
    },
    fromJSON = function(GamePlayPlayersJson) {
      GamePlayPlayersObject <- jsonlite::fromJSON(GamePlayPlayersJson)
      if (!is.null(GamePlayPlayersObject$`player`)) {
        playerObject <- GamePlayPlayer$new()
        playerObject$fromJSON(jsonlite::toJSON(GamePlayPlayersObject$player, auto_unbox = TRUE))
        self$`player` <- playerObject
      }
      if (!is.null(GamePlayPlayersObject$`playerType`)) {
        self$`playerType` <- GamePlayPlayersObject$`playerType`
      }
    },
    toJSONString = function() {
       sprintf(
        '{
           "player": %s,
           "playerType": %s
        }',
        self$`player`$toJSON(),
        self$`playerType`
      )
    },
    fromJSONString = function(GamePlayPlayersJson) {
      GamePlayPlayersObject <- jsonlite::fromJSON(GamePlayPlayersJson)
      GamePlayPlayerObject <- GamePlayPlayer$new()
      self$`player` <- GamePlayPlayerObject$fromJSON(jsonlite::toJSON(GamePlayPlayersObject$player, auto_unbox = TRUE))
      self$`playerType` <- GamePlayPlayersObject$`playerType`
    }
  )
)
