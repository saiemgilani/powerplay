# NHL API
#
# Documenting the publicly accessible portions of the NHL API.
#
# OpenAPI spec version: 1.0.0
# 
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' Roster Class
#'
#' @field person 
#' @field jerseyNumber 
#' @field position 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Roster <- R6::R6Class(
  'Roster',
  public = list(
    person = NULL,
    jerseyNumber = NULL,
    position = NULL,
    initialize = function(person, jerseyNumber, position){
      if (!missing(person)) {
        stopifnot(R6::is.R6(person))
        self$person <- person
      }
      if (!missing(jerseyNumber)) {
        stopifnot(is.numeric(jerseyNumber), length(jerseyNumber) == 1)
        stopifnot(R6::is.R6(jerseyNumber))
        self$jerseyNumber <- jerseyNumber
      }
      if (!missing(position)) {
        stopifnot(R6::is.R6(position))
        self$position <- position
      }
    },
    toJSON = function() {
      RosterObject <- list()
      if (!is.null(self$person)) {
        RosterObject[['person']] <- self$person$toJSON()
      }
      if (!is.null(self$jerseyNumber)) {
        RosterObject[['jerseyNumber']] <- self$jerseyNumber$toJSON()
      }
      if (!is.null(self$position)) {
        RosterObject[['position']] <- self$position$toJSON()
      }

      RosterObject
    },
    fromJSON = function(RosterJson) {
      RosterObject <- jsonlite::fromJSON(RosterJson)
      if (!is.null(RosterObject$person)) {
        personObject <- RosterPerson$new()
        personObject$fromJSON(jsonlite::toJSON(RosterObject$person, auto_unbox = TRUE))
        self$person <- personObject
      }
      if (!is.null(RosterObject$jerseyNumber)) {
        jerseyNumberObject <- BigDecimal$new()
        jerseyNumberObject$fromJSON(jsonlite::toJSON(RosterObject$jerseyNumber, auto_unbox = TRUE))
        self$jerseyNumber <- jerseyNumberObject
      }
      if (!is.null(RosterObject$position)) {
        positionObject <- DraftProspectPrimaryPosition$new()
        positionObject$fromJSON(jsonlite::toJSON(RosterObject$position, auto_unbox = TRUE))
        self$position <- positionObject
      }
    },
    toJSONString = function() {
       sprintf(
        '{
           "person": %s,
           "jerseyNumber": %s,
           "position": %s
        }',
        self$person$toJSON(),
        self$jerseyNumber$toJSON(),
        self$position$toJSON()
      )
    },
    fromJSONString = function(RosterJson) {
      RosterObject <- jsonlite::fromJSON(RosterJson)
      RosterPersonObject <- RosterPerson$new()
      self$person <- RosterPersonObject$fromJSON(jsonlite::toJSON(RosterObject$person, auto_unbox = TRUE))
      BigDecimalObject <- BigDecimal$new()
      self$jerseyNumber <- BigDecimalObject$fromJSON(jsonlite::toJSON(RosterObject$jerseyNumber, auto_unbox = TRUE))
      DraftProspectPrimaryPositionObject <- DraftProspectPrimaryPosition$new()
      self$position <- DraftProspectPrimaryPositionObject$fromJSON(jsonlite::toJSON(RosterObject$position, auto_unbox = TRUE))
    }
  )
)