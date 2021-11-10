# NHL API
#
# Documenting the publicly accessible portions of the NHL API.
#
# OpenAPI spec version: 1.0.0
# 
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' GameBoxscoreTeamPlayersPerson Class
#'
#' @field id 
#' @field fullName 
#' @field link 
#' @field shootsCatches 
#' @field rosterStatus 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
GameBoxscoreTeamPlayersPerson <- R6::R6Class(
  'GameBoxscoreTeamPlayersPerson',
  public = list(
    id = NULL,
    fullName = NULL,
    link = NULL,
    shootsCatches = NULL,
    rosterStatus = NULL,
    initialize = function(id, fullName, link, shootsCatches, rosterStatus){
      if (!missing(id)) {
        stopifnot(is.numeric(id), length(id) == 1)
        stopifnot(R6::is.R6(id))
        self$id <- id
      }
      if (!missing(fullName)) {
        stopifnot(is.character(fullName), length(fullName) == 1)
        self$fullName <- fullName
      }
      if (!missing(link)) {
        stopifnot(is.character(link), length(link) == 1)
        self$link <- link
      }
      if (!missing(shootsCatches)) {
        stopifnot(is.character(shootsCatches), length(shootsCatches) == 1)
        self$shootsCatches <- shootsCatches
      }
      if (!missing(rosterStatus)) {
        stopifnot(is.character(rosterStatus), length(rosterStatus) == 1)
        self$rosterStatus <- rosterStatus
      }
    },
    toJSON = function() {
      GameBoxscoreTeamPlayersPersonObject <- list()
      if (!is.null(self$id)) {
        GameBoxscoreTeamPlayersPersonObject[['id']] <- self$id$toJSON()
      }
      if (!is.null(self$fullName)) {
        GameBoxscoreTeamPlayersPersonObject[['fullName']] <- self$fullName
      }
      if (!is.null(self$link)) {
        GameBoxscoreTeamPlayersPersonObject[['link']] <- self$link
      }
      if (!is.null(self$shootsCatches)) {
        GameBoxscoreTeamPlayersPersonObject[['shootsCatches']] <- self$shootsCatches
      }
      if (!is.null(self$rosterStatus)) {
        GameBoxscoreTeamPlayersPersonObject[['rosterStatus']] <- self$rosterStatus
      }

      GameBoxscoreTeamPlayersPersonObject
    },
    fromJSON = function(GameBoxscoreTeamPlayersPersonJson) {
      GameBoxscoreTeamPlayersPersonObject <- jsonlite::fromJSON(GameBoxscoreTeamPlayersPersonJson)
      if (!is.null(GameBoxscoreTeamPlayersPersonObject$id)) {
        idObject <- BigDecimal$new()
        idObject$fromJSON(jsonlite::toJSON(GameBoxscoreTeamPlayersPersonObject$id, auto_unbox = TRUE))
        self$id <- idObject
      }
      if (!is.null(GameBoxscoreTeamPlayersPersonObject$fullName)) {
        self$fullName <- GameBoxscoreTeamPlayersPersonObject$fullName
      }
      if (!is.null(GameBoxscoreTeamPlayersPersonObject$link)) {
        self$link <- GameBoxscoreTeamPlayersPersonObject$link
      }
      if (!is.null(GameBoxscoreTeamPlayersPersonObject$shootsCatches)) {
        self$shootsCatches <- GameBoxscoreTeamPlayersPersonObject$shootsCatches
      }
      if (!is.null(GameBoxscoreTeamPlayersPersonObject$rosterStatus)) {
        self$rosterStatus <- GameBoxscoreTeamPlayersPersonObject$rosterStatus
      }
    },
    toJSONString = function() {
       sprintf(
        '{
           "id": %s,
           "fullName": %s,
           "link": %s,
           "shootsCatches": %s,
           "rosterStatus": %s
        }',
        self$id$toJSON(),
        self$fullName,
        self$link,
        self$shootsCatches,
        self$rosterStatus
      )
    },
    fromJSONString = function(GameBoxscoreTeamPlayersPersonJson) {
      GameBoxscoreTeamPlayersPersonObject <- jsonlite::fromJSON(GameBoxscoreTeamPlayersPersonJson)
      BigDecimalObject <- BigDecimal$new()
      self$id <- BigDecimalObject$fromJSON(jsonlite::toJSON(GameBoxscoreTeamPlayersPersonObject$id, auto_unbox = TRUE))
      self$fullName <- GameBoxscoreTeamPlayersPersonObject$fullName
      self$link <- GameBoxscoreTeamPlayersPersonObject$link
      self$shootsCatches <- GameBoxscoreTeamPlayersPersonObject$shootsCatches
      self$rosterStatus <- GameBoxscoreTeamPlayersPersonObject$rosterStatus
    }
  )
)