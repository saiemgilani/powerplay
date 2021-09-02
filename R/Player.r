# NHL API
#
# Documenting the publicly accessible portions of the NHL API.
#
# OpenAPI spec version: 1.0.0
# 
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' Player Class
#'
#' @field id 
#' @field fullName 
#' @field link 
#' @field firstName 
#' @field lastName 
#' @field primaryNumber 
#' @field birthDate 
#' @field currentAge 
#' @field birthCity 
#' @field birthStateProvince 
#' @field birthCountry 
#' @field nationality 
#' @field height 
#' @field weight 
#' @field active 
#' @field alternateCaptain 
#' @field captain 
#' @field rookie 
#' @field shootsCatches 
#' @field rosterStatus 
#' @field currentTeam 
#' @field primaryPosition 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Player <- R6::R6Class(
  'Player',
  public = list(
    `id` = NULL,
    `fullName` = NULL,
    `link` = NULL,
    `firstName` = NULL,
    `lastName` = NULL,
    `primaryNumber` = NULL,
    `birthDate` = NULL,
    `currentAge` = NULL,
    `birthCity` = NULL,
    `birthStateProvince` = NULL,
    `birthCountry` = NULL,
    `nationality` = NULL,
    `height` = NULL,
    `weight` = NULL,
    `active` = NULL,
    `alternateCaptain` = NULL,
    `captain` = NULL,
    `rookie` = NULL,
    `shootsCatches` = NULL,
    `rosterStatus` = NULL,
    `currentTeam` = NULL,
    `primaryPosition` = NULL,
    initialize = function(`id`, `fullName`, `link`, `firstName`, `lastName`, `primaryNumber`, `birthDate`, `currentAge`, `birthCity`, `birthStateProvince`, `birthCountry`, `nationality`, `height`, `weight`, `active`, `alternateCaptain`, `captain`, `rookie`, `shootsCatches`, `rosterStatus`, `currentTeam`, `primaryPosition`){
      if (!missing(`id`)) {
        stopifnot(is.numeric(`id`), length(`id`) == 1)
        stopifnot(R6::is.R6(`id`))
        self$`id` <- `id`
      }
      if (!missing(`fullName`)) {
        stopifnot(is.character(`fullName`), length(`fullName`) == 1)
        self$`fullName` <- `fullName`
      }
      if (!missing(`link`)) {
        stopifnot(is.character(`link`), length(`link`) == 1)
        self$`link` <- `link`
      }
      if (!missing(`firstName`)) {
        stopifnot(is.character(`firstName`), length(`firstName`) == 1)
        self$`firstName` <- `firstName`
      }
      if (!missing(`lastName`)) {
        stopifnot(is.character(`lastName`), length(`lastName`) == 1)
        self$`lastName` <- `lastName`
      }
      if (!missing(`primaryNumber`)) {
        stopifnot(is.character(`primaryNumber`), length(`primaryNumber`) == 1)
        self$`primaryNumber` <- `primaryNumber`
      }
      if (!missing(`birthDate`)) {
        stopifnot(is.character(`birthDate`), length(`birthDate`) == 1)
        self$`birthDate` <- `birthDate`
      }
      if (!missing(`currentAge`)) {
        stopifnot(is.numeric(`currentAge`), length(`currentAge`) == 1)
        stopifnot(R6::is.R6(`currentAge`))
        self$`currentAge` <- `currentAge`
      }
      if (!missing(`birthCity`)) {
        stopifnot(is.character(`birthCity`), length(`birthCity`) == 1)
        self$`birthCity` <- `birthCity`
      }
      if (!missing(`birthStateProvince`)) {
        stopifnot(is.character(`birthStateProvince`), length(`birthStateProvince`) == 1)
        self$`birthStateProvince` <- `birthStateProvince`
      }
      if (!missing(`birthCountry`)) {
        stopifnot(is.character(`birthCountry`), length(`birthCountry`) == 1)
        self$`birthCountry` <- `birthCountry`
      }
      if (!missing(`nationality`)) {
        stopifnot(is.character(`nationality`), length(`nationality`) == 1)
        self$`nationality` <- `nationality`
      }
      if (!missing(`height`)) {
        stopifnot(is.character(`height`), length(`height`) == 1)
        self$`height` <- `height`
      }
      if (!missing(`weight`)) {
        stopifnot(is.numeric(`weight`), length(`weight`) == 1)
        stopifnot(R6::is.R6(`weight`))
        self$`weight` <- `weight`
      }
      if (!missing(`active`)) {
        self$`active` <- `active`
      }
      if (!missing(`alternateCaptain`)) {
        self$`alternateCaptain` <- `alternateCaptain`
      }
      if (!missing(`captain`)) {
        self$`captain` <- `captain`
      }
      if (!missing(`rookie`)) {
        self$`rookie` <- `rookie`
      }
      if (!missing(`shootsCatches`)) {
        stopifnot(is.character(`shootsCatches`), length(`shootsCatches`) == 1)
        self$`shootsCatches` <- `shootsCatches`
      }
      if (!missing(`rosterStatus`)) {
        stopifnot(is.character(`rosterStatus`), length(`rosterStatus`) == 1)
        self$`rosterStatus` <- `rosterStatus`
      }
      if (!missing(`currentTeam`)) {
        stopifnot(R6::is.R6(`currentTeam`))
        self$`currentTeam` <- `currentTeam`
      }
      if (!missing(`primaryPosition`)) {
        stopifnot(R6::is.R6(`primaryPosition`))
        self$`primaryPosition` <- `primaryPosition`
      }
    },
    toJSON = function() {
      PlayerObject <- list()
      if (!is.null(self$`id`)) {
        PlayerObject[['id']] <- self$`id`$toJSON()
      }
      if (!is.null(self$`fullName`)) {
        PlayerObject[['fullName']] <- self$`fullName`
      }
      if (!is.null(self$`link`)) {
        PlayerObject[['link']] <- self$`link`
      }
      if (!is.null(self$`firstName`)) {
        PlayerObject[['firstName']] <- self$`firstName`
      }
      if (!is.null(self$`lastName`)) {
        PlayerObject[['lastName']] <- self$`lastName`
      }
      if (!is.null(self$`primaryNumber`)) {
        PlayerObject[['primaryNumber']] <- self$`primaryNumber`
      }
      if (!is.null(self$`birthDate`)) {
        PlayerObject[['birthDate']] <- self$`birthDate`
      }
      if (!is.null(self$`currentAge`)) {
        PlayerObject[['currentAge']] <- self$`currentAge`$toJSON()
      }
      if (!is.null(self$`birthCity`)) {
        PlayerObject[['birthCity']] <- self$`birthCity`
      }
      if (!is.null(self$`birthStateProvince`)) {
        PlayerObject[['birthStateProvince']] <- self$`birthStateProvince`
      }
      if (!is.null(self$`birthCountry`)) {
        PlayerObject[['birthCountry']] <- self$`birthCountry`
      }
      if (!is.null(self$`nationality`)) {
        PlayerObject[['nationality']] <- self$`nationality`
      }
      if (!is.null(self$`height`)) {
        PlayerObject[['height']] <- self$`height`
      }
      if (!is.null(self$`weight`)) {
        PlayerObject[['weight']] <- self$`weight`$toJSON()
      }
      if (!is.null(self$`active`)) {
        PlayerObject[['active']] <- self$`active`
      }
      if (!is.null(self$`alternateCaptain`)) {
        PlayerObject[['alternateCaptain']] <- self$`alternateCaptain`
      }
      if (!is.null(self$`captain`)) {
        PlayerObject[['captain']] <- self$`captain`
      }
      if (!is.null(self$`rookie`)) {
        PlayerObject[['rookie']] <- self$`rookie`
      }
      if (!is.null(self$`shootsCatches`)) {
        PlayerObject[['shootsCatches']] <- self$`shootsCatches`
      }
      if (!is.null(self$`rosterStatus`)) {
        PlayerObject[['rosterStatus']] <- self$`rosterStatus`
      }
      if (!is.null(self$`currentTeam`)) {
        PlayerObject[['currentTeam']] <- self$`currentTeam`$toJSON()
      }
      if (!is.null(self$`primaryPosition`)) {
        PlayerObject[['primaryPosition']] <- self$`primaryPosition`$toJSON()
      }

      PlayerObject
    },
    fromJSON = function(PlayerJson) {
      PlayerObject <- jsonlite::fromJSON(PlayerJson)
      if (!is.null(PlayerObject$`id`)) {
        idObject <- BigDecimal$new()
        idObject$fromJSON(jsonlite::toJSON(PlayerObject$id, auto_unbox = TRUE))
        self$`id` <- idObject
      }
      if (!is.null(PlayerObject$`fullName`)) {
        self$`fullName` <- PlayerObject$`fullName`
      }
      if (!is.null(PlayerObject$`link`)) {
        self$`link` <- PlayerObject$`link`
      }
      if (!is.null(PlayerObject$`firstName`)) {
        self$`firstName` <- PlayerObject$`firstName`
      }
      if (!is.null(PlayerObject$`lastName`)) {
        self$`lastName` <- PlayerObject$`lastName`
      }
      if (!is.null(PlayerObject$`primaryNumber`)) {
        self$`primaryNumber` <- PlayerObject$`primaryNumber`
      }
      if (!is.null(PlayerObject$`birthDate`)) {
        self$`birthDate` <- PlayerObject$`birthDate`
      }
      if (!is.null(PlayerObject$`currentAge`)) {
        currentAgeObject <- BigDecimal$new()
        currentAgeObject$fromJSON(jsonlite::toJSON(PlayerObject$currentAge, auto_unbox = TRUE))
        self$`currentAge` <- currentAgeObject
      }
      if (!is.null(PlayerObject$`birthCity`)) {
        self$`birthCity` <- PlayerObject$`birthCity`
      }
      if (!is.null(PlayerObject$`birthStateProvince`)) {
        self$`birthStateProvince` <- PlayerObject$`birthStateProvince`
      }
      if (!is.null(PlayerObject$`birthCountry`)) {
        self$`birthCountry` <- PlayerObject$`birthCountry`
      }
      if (!is.null(PlayerObject$`nationality`)) {
        self$`nationality` <- PlayerObject$`nationality`
      }
      if (!is.null(PlayerObject$`height`)) {
        self$`height` <- PlayerObject$`height`
      }
      if (!is.null(PlayerObject$`weight`)) {
        weightObject <- BigDecimal$new()
        weightObject$fromJSON(jsonlite::toJSON(PlayerObject$weight, auto_unbox = TRUE))
        self$`weight` <- weightObject
      }
      if (!is.null(PlayerObject$`active`)) {
        self$`active` <- PlayerObject$`active`
      }
      if (!is.null(PlayerObject$`alternateCaptain`)) {
        self$`alternateCaptain` <- PlayerObject$`alternateCaptain`
      }
      if (!is.null(PlayerObject$`captain`)) {
        self$`captain` <- PlayerObject$`captain`
      }
      if (!is.null(PlayerObject$`rookie`)) {
        self$`rookie` <- PlayerObject$`rookie`
      }
      if (!is.null(PlayerObject$`shootsCatches`)) {
        self$`shootsCatches` <- PlayerObject$`shootsCatches`
      }
      if (!is.null(PlayerObject$`rosterStatus`)) {
        self$`rosterStatus` <- PlayerObject$`rosterStatus`
      }
      if (!is.null(PlayerObject$`currentTeam`)) {
        currentTeamObject <- PlayerCurrentTeam$new()
        currentTeamObject$fromJSON(jsonlite::toJSON(PlayerObject$currentTeam, auto_unbox = TRUE))
        self$`currentTeam` <- currentTeamObject
      }
      if (!is.null(PlayerObject$`primaryPosition`)) {
        primaryPositionObject <- DraftProspectPrimaryPosition$new()
        primaryPositionObject$fromJSON(jsonlite::toJSON(PlayerObject$primaryPosition, auto_unbox = TRUE))
        self$`primaryPosition` <- primaryPositionObject
      }
    },
    toJSONString = function() {
       sprintf(
        '{
           "id": %s,
           "fullName": %s,
           "link": %s,
           "firstName": %s,
           "lastName": %s,
           "primaryNumber": %s,
           "birthDate": %s,
           "currentAge": %s,
           "birthCity": %s,
           "birthStateProvince": %s,
           "birthCountry": %s,
           "nationality": %s,
           "height": %s,
           "weight": %s,
           "active": %s,
           "alternateCaptain": %s,
           "captain": %s,
           "rookie": %s,
           "shootsCatches": %s,
           "rosterStatus": %s,
           "currentTeam": %s,
           "primaryPosition": %s
        }',
        self$`id`$toJSON(),
        self$`fullName`,
        self$`link`,
        self$`firstName`,
        self$`lastName`,
        self$`primaryNumber`,
        self$`birthDate`,
        self$`currentAge`$toJSON(),
        self$`birthCity`,
        self$`birthStateProvince`,
        self$`birthCountry`,
        self$`nationality`,
        self$`height`,
        self$`weight`$toJSON(),
        self$`active`,
        self$`alternateCaptain`,
        self$`captain`,
        self$`rookie`,
        self$`shootsCatches`,
        self$`rosterStatus`,
        self$`currentTeam`$toJSON(),
        self$`primaryPosition`$toJSON()
      )
    },
    fromJSONString = function(PlayerJson) {
      PlayerObject <- jsonlite::fromJSON(PlayerJson)
      BigDecimalObject <- BigDecimal$new()
      self$`id` <- BigDecimalObject$fromJSON(jsonlite::toJSON(PlayerObject$id, auto_unbox = TRUE))
      self$`fullName` <- PlayerObject$`fullName`
      self$`link` <- PlayerObject$`link`
      self$`firstName` <- PlayerObject$`firstName`
      self$`lastName` <- PlayerObject$`lastName`
      self$`primaryNumber` <- PlayerObject$`primaryNumber`
      self$`birthDate` <- PlayerObject$`birthDate`
      BigDecimalObject <- BigDecimal$new()
      self$`currentAge` <- BigDecimalObject$fromJSON(jsonlite::toJSON(PlayerObject$currentAge, auto_unbox = TRUE))
      self$`birthCity` <- PlayerObject$`birthCity`
      self$`birthStateProvince` <- PlayerObject$`birthStateProvince`
      self$`birthCountry` <- PlayerObject$`birthCountry`
      self$`nationality` <- PlayerObject$`nationality`
      self$`height` <- PlayerObject$`height`
      BigDecimalObject <- BigDecimal$new()
      self$`weight` <- BigDecimalObject$fromJSON(jsonlite::toJSON(PlayerObject$weight, auto_unbox = TRUE))
      self$`active` <- PlayerObject$`active`
      self$`alternateCaptain` <- PlayerObject$`alternateCaptain`
      self$`captain` <- PlayerObject$`captain`
      self$`rookie` <- PlayerObject$`rookie`
      self$`shootsCatches` <- PlayerObject$`shootsCatches`
      self$`rosterStatus` <- PlayerObject$`rosterStatus`
      PlayerCurrentTeamObject <- PlayerCurrentTeam$new()
      self$`currentTeam` <- PlayerCurrentTeamObject$fromJSON(jsonlite::toJSON(PlayerObject$currentTeam, auto_unbox = TRUE))
      DraftProspectPrimaryPositionObject <- DraftProspectPrimaryPosition$new()
      self$`primaryPosition` <- DraftProspectPrimaryPositionObject$fromJSON(jsonlite::toJSON(PlayerObject$primaryPosition, auto_unbox = TRUE))
    }
  )
)
