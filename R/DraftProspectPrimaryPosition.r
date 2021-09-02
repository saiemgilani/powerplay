# NHL API
#
# Documenting the publicly accessible portions of the NHL API.
#
# OpenAPI spec version: 1.0.0
# 
# Generated by: https://github.com/swagger-api/swagger-codegen.git

#' DraftProspectPrimaryPosition Class
#'
#' @field code 
#' @field name 
#' @field type 
#' @field abbreviation 
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
DraftProspectPrimaryPosition <- R6::R6Class(
  'DraftProspectPrimaryPosition',
  public = list(
    `code` = NULL,
    `name` = NULL,
    `type` = NULL,
    `abbreviation` = NULL,
    initialize = function(`code`, `name`, `type`, `abbreviation`){
      if (!missing(`code`)) {
        stopifnot(is.character(`code`), length(`code`) == 1)
        self$`code` <- `code`
      }
      if (!missing(`name`)) {
        stopifnot(is.character(`name`), length(`name`) == 1)
        self$`name` <- `name`
      }
      if (!missing(`type`)) {
        stopifnot(is.character(`type`), length(`type`) == 1)
        self$`type` <- `type`
      }
      if (!missing(`abbreviation`)) {
        stopifnot(is.character(`abbreviation`), length(`abbreviation`) == 1)
        self$`abbreviation` <- `abbreviation`
      }
    },
    toJSON = function() {
      DraftProspectPrimaryPositionObject <- list()
      if (!is.null(self$`code`)) {
        DraftProspectPrimaryPositionObject[['code']] <- self$`code`
      }
      if (!is.null(self$`name`)) {
        DraftProspectPrimaryPositionObject[['name']] <- self$`name`
      }
      if (!is.null(self$`type`)) {
        DraftProspectPrimaryPositionObject[['type']] <- self$`type`
      }
      if (!is.null(self$`abbreviation`)) {
        DraftProspectPrimaryPositionObject[['abbreviation']] <- self$`abbreviation`
      }

      DraftProspectPrimaryPositionObject
    },
    fromJSON = function(DraftProspectPrimaryPositionJson) {
      DraftProspectPrimaryPositionObject <- jsonlite::fromJSON(DraftProspectPrimaryPositionJson)
      if (!is.null(DraftProspectPrimaryPositionObject$`code`)) {
        self$`code` <- DraftProspectPrimaryPositionObject$`code`
      }
      if (!is.null(DraftProspectPrimaryPositionObject$`name`)) {
        self$`name` <- DraftProspectPrimaryPositionObject$`name`
      }
      if (!is.null(DraftProspectPrimaryPositionObject$`type`)) {
        self$`type` <- DraftProspectPrimaryPositionObject$`type`
      }
      if (!is.null(DraftProspectPrimaryPositionObject$`abbreviation`)) {
        self$`abbreviation` <- DraftProspectPrimaryPositionObject$`abbreviation`
      }
    },
    toJSONString = function() {
       sprintf(
        '{
           "code": %s,
           "name": %s,
           "type": %s,
           "abbreviation": %s
        }',
        self$`code`,
        self$`name`,
        self$`type`,
        self$`abbreviation`
      )
    },
    fromJSONString = function(DraftProspectPrimaryPositionJson) {
      DraftProspectPrimaryPositionObject <- jsonlite::fromJSON(DraftProspectPrimaryPositionJson)
      self$`code` <- DraftProspectPrimaryPositionObject$`code`
      self$`name` <- DraftProspectPrimaryPositionObject$`name`
      self$`type` <- DraftProspectPrimaryPositionObject$`type`
      self$`abbreviation` <- DraftProspectPrimaryPositionObject$`abbreviation`
    }
  )
)
