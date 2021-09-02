# NHL API
#
# Documenting the publicly accessible portions of the NHL API.
#
# OpenAPI spec version: 1.0.0
# 
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' VenueTimeZone Class
#'
#' @field id 
#' @field offset 
#' @field tz 
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
VenueTimeZone <- R6::R6Class(
  'VenueTimeZone',
  public = list(
    id = NULL,
    offset = NULL,
    tz = NULL,
    #' @description
    #' Create a new VenueTimeZone object.
    #' @param id id
    #' @param offset offset 
    #' @param tz tz 
    #' @return A new `VenueTimeZone` object.
    initialize = function(id, offset, tz){
      if (!missing(id)) {
        stopifnot(is.character(id), length(id) == 1)
        self$id <- id
      }
      if (!missing(offset)) {
        stopifnot(is.numeric(offset), length(offset) == 1)
        stopifnot(R6::is.R6(offset))
        self$offset <- offset
      }
      if (!missing(tz)) {
        stopifnot(is.character(tz), length(tz) == 1)
        self$tz <- tz
      }
    },
    #' @description to JSON
    toJSON = function() {
      VenueTimeZoneObject <- list()
      if (!is.null(self$id)) {
        VenueTimeZoneObject[['id']] <- self$id
      }
      if (!is.null(self$offset)) {
        VenueTimeZoneObject[['offset']] <- self$offset$toJSON()
      }
      if (!is.null(self$tz)) {
        VenueTimeZoneObject[['tz']] <- self$tz
      }

      VenueTimeZoneObject
    },
    #' @description from JSON
    #' @param VenueTimeZoneJson VenueTimeZoneJson
    fromJSON = function(VenueTimeZoneJson=NULL) {
      VenueTimeZoneObject <- jsonlite::fromJSON(VenueTimeZoneJson)
      if (!is.null(VenueTimeZoneObject$id)) {
        self$id <- VenueTimeZoneObject$id
      }
      if (!is.null(VenueTimeZoneObject$offset)) {
        offsetObject <- BigDecimal$new()
        offsetObject$fromJSON(jsonlite::toJSON(VenueTimeZoneObject$offset, auto_unbox = TRUE))
        self$offset <- offsetObject
      }
      if (!is.null(VenueTimeZoneObject$tz)) {
        self$tz <- VenueTimeZoneObject$tz
      }
    },
    #' @description to JSON string
    toJSONString = function() {
       sprintf(
        '{
           "id": %s,
           "offset": %s,
           "tz": %s
        }',
        self$id,
        self$offset$toJSON(),
        self$tz
      )
    },
    #' @description from JSON string
    #' @param VenueTimeZoneJson VenueTimeZoneJson
    fromJSONString = function(VenueTimeZoneJson=NULL) {
      VenueTimeZoneObject <- jsonlite::fromJSON(VenueTimeZoneJson)
      self$id <- VenueTimeZoneObject$id
      BigDecimalObject <- BigDecimal$new()
      self$offset <- BigDecimalObject$fromJSON(jsonlite::toJSON(VenueTimeZoneObject$offset, auto_unbox = TRUE))
      self$tz <- VenueTimeZoneObject$tz
    }
  )
)
