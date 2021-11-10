# NHL API
#
# Documenting the publicly accessible portions of the NHL API.
#
# OpenAPI spec version: 1.0.0
# 
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' Standings Class
#'
#' @field copyright 
#' @field records 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Standings <- R6::R6Class(
  'Standings',
  public = list(
    copyright = NULL,
    records = NULL,
    initialize = function(copyright, records){
      if (!missing(copyright)) {
        stopifnot(is.character(copyright), length(copyright) == 1)
        self$copyright <- copyright
      }
      if (!missing(records)) {
        stopifnot(is.list(records), length(records) != 0)
        lapply(records, function(x) stopifnot(R6::is.R6(x)))
        self$records <- records
      }
    },
    toJSON = function() {
      StandingsObject <- list()
      if (!is.null(self$copyright)) {
        StandingsObject[['copyright']] <- self$copyright
      }
      if (!is.null(self$records)) {
        StandingsObject[['records']] <- lapply(self$records, function(x) x$toJSON())
      }

      StandingsObject
    },
    fromJSON = function(StandingsJson) {
      StandingsObject <- jsonlite::fromJSON(StandingsJson)
      if (!is.null(StandingsObject$copyright)) {
        self$copyright <- StandingsObject$copyright
      }
      if (!is.null(StandingsObject$records)) {
        self$records <- lapply(StandingsObject$records, function(x) {
          recordsObject <- StandingsRecords$new()
          recordsObject$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE))
          recordsObject
        })
      }
    },
    toJSONString = function() {
       sprintf(
        '{
           "copyright": %s,
           "records": [%s]
        }',
        self$copyright,
        lapply(self$records, function(x) paste(x$toJSON(), sep=","))
      )
    },
    fromJSONString = function(StandingsJson) {
      StandingsObject <- jsonlite::fromJSON(StandingsJson)
      self$copyright <- StandingsObject$copyright
      self$records <- lapply(StandingsObject$records, function(x) StandingsRecords$new()$fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE)))
    }
  )
)